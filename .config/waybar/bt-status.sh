#!/usr/bin/env bash

status=$(/usr/bin/bluetoothctl -- show E8:B1:FC:CC:E6:23 | grep 'Powered: yes')
if [[ -n "$status" ]]; then
  echo '{"text":"","class":"active"}'
else
  echo '{"text":"","class":"not-active"}'
fi
