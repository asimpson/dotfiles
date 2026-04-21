#!/bin/bash

usage() {
  cat <<'EOF'
Usage: sendPatch [-h|--help]

Email the most recent git commit (HEAD) as a patch to the host for review.

Runs `git format-patch -1 --stdout` on HEAD and pipes the result to the
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

case "''${1:-}" in
  -h|--help) usage; exit 0 ;;
  "") ;;
  *) echo "sendPatch: unknown argument: $1" >&2; usage >&2; exit 2 ;;
esac

git format-patch -1 --stdout | curl -s --url smtp://192.168.122.1:25 --mail-from agent@llm-jail --mail-rcpt patches@localhost -T -
