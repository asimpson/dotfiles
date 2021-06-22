#!/bin/sh

hdmiConnected=$(xrandr | grep -qs "HDMI-2 connected ")
code=$?

if [ "${code}" -eq 0 ]; then
  xrandr --output eDP-1 --off
else
  xrandr --output eDP-1 --auto
fi
