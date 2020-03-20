#!/usr/bin/env bash

status=$(cat /sys/devices/system/cpu/intel_pstate/no_turbo)
if [[ "$status" == 0 ]]; then
  echo " 🐇"
else
  echo " 🐢"
fi
