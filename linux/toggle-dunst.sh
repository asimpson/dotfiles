#!/bin/sh


if [ -f "/tmp/dunst-muted" ]; then
  notify-send "DUNST_COMMAND_RESUME";
  rm /tmp/dunst-muted;
else
  notify-send "DUNST_COMMAND_PAUSE";
  touch /tmp/dunst-muted;
fi
