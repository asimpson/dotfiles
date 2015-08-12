#!/bin/bash
#strict mode
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

killInsertPic () {
  if ps aux | grep "InsertPic" | grep -v grep > /dev/null; then
    syslog -s -l ``No'' InsertPic: Restarted by insertPic.sh
    killall "InsertPic"
    open /Applications/InsertPic.app
  fi
}

killInsertPic
