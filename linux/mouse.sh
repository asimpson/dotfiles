#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash

# Store the choices in an array
choices=("lofibar" "mpd" "Play/Pause Apple Music" "Skip/Next")

# Get Apple Music Chrome app instance
app_id="kjbdgfilnfhdoflbpgamdcdgpehopbep"
process="chromium.instance$(ps x | rg ${app_id} | rg -v "rg" | xargs | cut -f 1 -d ' ')"

# Present choices using rofi and store the selection
selected=$(printf '%s\n' "${choices[@]}" | rofi -dmenu -i -p "Media Control")

case "$selected" in
    "lofibar")
        playerctl play-pause --player lofibar
        ;;
    "mpd")
        mpc toggle
        ;;
    "Play/Pause Apple Music")
        playerctl play-pause --player "${process}"
        dunstify "⏯️" "$(playerctl metadata -f '{{title}} - {{artist}}')"
        ;;
    "Skip/Next")
        playerctl next --player "${process}"
        dunstify "$(playerctl metadata -f '{{title}} - {{artist}}')"
        ;;
    *)
        # If no valid selection was made, exit
        exit 1
        ;;
esac
