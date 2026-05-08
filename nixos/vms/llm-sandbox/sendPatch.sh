#!/bin/bash

set -euo pipefail

hash_version="v1"
hash_hex_chars=48

usage() {
  cat <<'EOF'
Usage: sendPatch [-h|--help] [--session-id <id> --cwd <path>]

Email the most recent git commit (HEAD) as a patch to the host for review.

Runs `git format-patch -1 --stdout` on HEAD and sends the result via the
host's SMTP server (192.168.122.1:25), delivering it to patches@localhost.

If --session-id and --cwd are provided, sendPatch also adds a Reply-To header
in the form patches+<hash>@localhost, where hash = SHA256("v1\0session\0cwd")
truncated to 48 hex chars. Replies can then be routed back into that pi session.

Typical workflow:
  1. Make your changes in a git repo under /tmp.
  2. git add -A && git commit -m "your message"
  3. sendPatch

For multi-commit changes, commit each change separately and run sendPatch
once per commit (one email per commit).

Options:
  -h, --help            Show this help message and exit.
  --session-id <id>     pi session id used for reply routing hash.
  --cwd <path>          Session/project cwd used for reply routing hash.
EOF
}

cleanup() {
  if [[ -n "${patch_file:-}" ]]; then
    rm -f "$patch_file"
  fi
}

canonicalize_cwd() {
  local input="$1"
  (cd "$input" && pwd -P)
}

compute_session_hash() {
  local session_id="$1"
  local session_cwd="$2"
  printf '%s\0%s\0%s' "$hash_version" "$session_id" "$session_cwd" \
    | sha256sum \
    | awk '{print $1}' \
    | cut -c1-"$hash_hex_chars"
}

session_id=""
session_cwd=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)
      usage
      exit 0
      ;;
    --session-id)
      if [[ $# -lt 2 ]]; then
        echo "sendPatch: --session-id requires a value" >&2
        usage >&2
        exit 2
      fi
      session_id="$2"
      shift 2
      ;;
    --cwd)
      if [[ $# -lt 2 ]]; then
        echo "sendPatch: --cwd requires a value" >&2
        usage >&2
        exit 2
      fi
      session_cwd="$2"
      shift 2
      ;;
    *)
      echo "sendPatch: unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [[ -n "$session_id" || -n "$session_cwd" ]]; then
  if [[ -z "$session_id" || -z "$session_cwd" ]]; then
    echo "sendPatch: --session-id and --cwd must be provided together" >&2
    usage >&2
    exit 2
  fi
  session_cwd="$(canonicalize_cwd "$session_cwd")"
  session_hash="$(compute_session_hash "$session_id" "$session_cwd")"
  reply_to="patches+${session_hash}@localhost"
fi

patch_file=$(mktemp)
trap cleanup EXIT

format_patch_args=(-1 --stdout)
if [[ -n "${reply_to:-}" ]]; then
  format_patch_args+=(
    --add-header "Reply-To: ${reply_to}"
    --add-header "X-Pi-Session-Hash: ${session_hash}"
    --add-header "X-Pi-Session-Id: ${session_id}"
    --add-header "X-Pi-Session-Cwd: ${session_cwd}"
  )
fi

git format-patch "${format_patch_args[@]}" >"$patch_file"

curl --silent --show-error \
  --url smtp://192.168.122.1:25 \
  --mail-from agent@llm-jail \
  --mail-rcpt patches@localhost \
  -T "$patch_file"

if [[ -n "${reply_to:-}" ]]; then
  printf 'sendPatch: reply routing %s\n' "$reply_to"
fi
