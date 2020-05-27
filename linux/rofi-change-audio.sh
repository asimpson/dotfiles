#!/bin/bash

source="$(pactl list short sinks | cut -f 2 | rofi -dpi 1 -dmenu -p "Change audio:")";
inputs="$(pactl list sink-inputs short | cut -f 1)";

for input in $inputs; do
  pactl move-sink-input "$input" "$source";
done

pactl set-default-sink "$source";
