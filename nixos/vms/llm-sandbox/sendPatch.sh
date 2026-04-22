#!/bin/bash

set -euo pipefail

usage() {
  cat <<'EOF'
Usage: sendPatch [-h|--help]

Email the most recent git commit (HEAD) as a patch to the host for review.

Runs `git format-patch -1 --stdout` on HEAD and sends the result via the
host's SMTP server (192.168.122.1:25), delivering it to patches@localhost.

Typical workflow:
  1. Make your changes in a git repo under /tmp.
  2. git add -A && git commit -m "your message"
  3. sendPatch

For multi-commit changes, commit each change separately and run sendPatch
once per commit (one email per commit).

Options:
  -h, --help    Show this help message and exit.
EOF
}

cleanup() {
  if [[ -n "${patch_file:-}" ]]; then
    rm -f "$patch_file"
  fi
}

case "${1:-}" in
  -h|--help) usage; exit 0 ;;
  "") ;;
  *) echo "sendPatch: unknown argument: $1" >&2; usage >&2; exit 2 ;;
esac

patch_file=$(mktemp)
trap cleanup EXIT

git format-patch -1 --stdout >"$patch_file"

curl --silent --show-error \
  --url smtp://192.168.122.1:25 \
  --mail-from agent@llm-jail \
  --mail-rcpt patches@localhost \
  -T "$patch_file"
