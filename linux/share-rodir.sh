#!/usr/bin/env bash
# BASE is strictly validated before being expanded into remote commands.
# shellcheck disable=SC2029
set -euo pipefail

DOMAIN="llm-jail"
GUEST="llmjail"
LIBVIRT_URI="qemu:///system"

usage() {
  cat >&2 <<EOF
Usage:
  ${0##*/} DIR
  ${0##*/} eject DIR
  ${0##*/} DIR eject

Attach DIR to ${DOMAIN}, mount it read-only in ${GUEST}, and clone it to
/tmp/BASENAME. The eject form unmounts the share and detaches the virtiofs
device; it leaves the writable clone in /tmp/BASENAME intact.
EOF
}

ACTION="attach"
case $# in
  1)
    INPUT_DIR=$1
    ;;
  2)
    if [[ $1 == "eject" || $1 == "--eject" ]]; then
      ACTION="eject"
      INPUT_DIR=$2
    elif [[ $2 == "eject" || $2 == "--eject" ]]; then
      ACTION="eject"
      INPUT_DIR=$1
    else
      usage
      exit 2
    fi
    ;;
  *)
    usage
    exit 2
    ;;
esac

TRIMMED_DIR=${INPUT_DIR%/}
[[ -n $TRIMMED_DIR ]] || TRIMMED_DIR=/
BASE=$(basename -- "$TRIMMED_DIR")

if [[ $ACTION == "attach" ]]; then
  if [[ ! -d $INPUT_DIR ]]; then
    echo "Not a directory: ${INPUT_DIR}" >&2
    exit 2
  fi
  DIR=$(realpath -- "$INPUT_DIR")

  if ! GIT_DIR=$(git -C "$DIR" rev-parse --absolute-git-dir 2>/dev/null); then
    echo "Not a Git repository: ${DIR}" >&2
    exit 2
  fi
  case $GIT_DIR in
    "$DIR"|"$DIR"/*)
      ;;
    *)
      echo "The repository's Git directory is outside the shared directory:" >&2
      echo "  work tree: ${DIR}" >&2
      echo "  Git dir:   ${GIT_DIR}" >&2
      echo "Use a standalone clone, or share the external Git directory as well." >&2
      exit 2
      ;;
  esac
else
  # Eject can still be attempted if the source was removed after attachment.
  DIR=$(realpath -m -- "$INPUT_DIR")
fi

xml_escape() {
  sed \
    -e 's/&/\&amp;/g' \
    -e 's/</\&lt;/g' \
    -e 's/>/\&gt;/g' \
    -e 's/"/\&quot;/g' \
    -e "s/'/\\&apos;/g"
}

XML_DIR=$(printf '%s' "$DIR" | xml_escape)
DEVICE_XML=$(mktemp)
trap 'rm -f -- "$DEVICE_XML"' EXIT

cat >"$DEVICE_XML" <<EOF
<filesystem type="mount" accessmode="passthrough">
  <driver type="virtiofs"/>
  <source dir="${XML_DIR}"/>
  <target dir="${BASE}"/>
  <readonly/>
</filesystem>
EOF

unmount_guest_share() {
  ssh "$GUEST" "
    set -eu
    mountpoint=/mnt/${BASE}
    if mountpoint -q -- \"\$mountpoint\"; then
      fstype=\$(findmnt -n -o FSTYPE --target \"\$mountpoint\")
      if [ \"\$fstype\" != virtiofs ]; then
        echo \"Refusing to unmount non-virtiofs filesystem at \$mountpoint\" >&2
        exit 1
      fi
      sudo -n umount -- \"\$mountpoint\"
    fi
  "
}

if [[ $ACTION == "eject" ]]; then
  # A mounted virtiofs share must be unmounted before its device is detached.
  unmount_guest_share

  if ! virsh -c "$LIBVIRT_URI" detach-device "$DOMAIN" \
    --live --file "$DEVICE_XML"; then
    echo "Failed to detach virtiofs target ${BASE}." >&2
    echo "Use the same DIR that was passed when the share was attached." >&2
    exit 1
  fi

  # Remove only the empty mountpoint, never its contents recursively.
  ssh "$GUEST" "sudo -n rmdir -- /mnt/${BASE} 2>/dev/null || true"
  echo "Ejected ${DIR}; writable clone /tmp/${BASE} was left intact"
  exit 0
fi

if ! virsh -c "$LIBVIRT_URI" attach-device "$DOMAIN" \
  --live --file "$DEVICE_XML"; then
  echo "Failed to attach ${DIR} as virtiofs target ${BASE}." >&2
  echo "If that target is already attached, eject it first:" >&2
  printf '  %q eject %q\n' "${0##*/}" "$DIR" >&2
  exit 1
fi

if ! ssh "$GUEST" "
  set -eu
  mountpoint=/mnt/${BASE}
  sudo -n mkdir -p -- \"\$mountpoint\"
  if mountpoint -q -- \"\$mountpoint\"; then
    fstype=\$(findmnt -n -o FSTYPE --target \"\$mountpoint\")
    if [ \"\$fstype\" != virtiofs ]; then
      echo \"Refusing to use non-virtiofs filesystem at \$mountpoint\" >&2
      exit 1
    fi
  else
    sudo -n mount -t virtiofs -o ro ${BASE} \"\$mountpoint\"
  fi
"; then
  echo "Failed to mount virtiofs target ${BASE} in ${GUEST}" >&2
  echo "Run '${0##*/} eject ${DIR}' to detach it." >&2
  exit 1
fi

if ! ssh "$GUEST" "
  set -eu
  sudo -n rm -rf -- /tmp/${BASE}
  git -c safe.directory=/mnt/${BASE} clone --no-hardlinks \
    /mnt/${BASE} /tmp/${BASE}
  git -C /tmp/${BASE} rev-parse --git-dir >/dev/null
"; then
  echo "Failed to clone /mnt/${BASE} to /tmp/${BASE}" >&2
  exit 1
fi

echo "Attached ${DIR} and cloned it to /tmp/${BASE}"
