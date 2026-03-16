#!/bin/sh

current="$(i3-msg -t get_workspaces | jq -r '.[] | select(.focused) | .name')"

# strip existing name after "N: " prefix, keeping just the number
num="$(echo "$current" | grep -oP '^\d+')"

name="$(rofi -dpi 1 -dmenu -p "Rename workspace:" -filter "")"

if [ -z "$name" ]; then
  # empty input resets to just the number
  i3-msg "rename workspace to \"$num\""
else
  i3-msg "rename workspace to \"$num: $name\""
fi
