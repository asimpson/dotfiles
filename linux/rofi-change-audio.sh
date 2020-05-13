#!/bin/bash

source=$(pactl list short sinks | cut -f 2 | rofi -dpi 1 -dmenu -p 'Change audio:');
input=$(pactl list sink-inputs short | cut -f 1)
pactl move-sink-input "$input" "$source";
pactl set-default-sink "$source";
