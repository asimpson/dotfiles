#!/usr/bin/env bash

status=$(/usr/bin/bluetoothctl -- show E8:B1:FC:CC:E6:23 | grep 'Powered: yes')
if [[ -n "$status" ]]; then
    /usr/bin/bluetoothctl -- power off
else
    /usr/bin/bluetoothctl -- power on
fi
