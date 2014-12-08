#!/bin/bash
ps aux | grep "AAM" | grep -v grep > /dev/null
if [ $? -eq 0 ]; then
  killall "AAM Updates Notifier"
fi

if [ $? -eq 1 ]; then
  echo "AAM is not running"
fi
