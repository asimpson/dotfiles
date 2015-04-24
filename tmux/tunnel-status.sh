#!/usr/bin/env zsh
ACTIVITY=`ps -wwajx | grep ssh | grep 8080`

if [ -z "$ACTIVITY" ]
then
  echo ""
else
  echo '⚡️'
fi
