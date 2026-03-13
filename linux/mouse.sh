#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash

# Store the choices in an array
choices=("⏯️ lofibar" "⏯️ 🍎 music" "⏭️ next" "🔅 set brightness")

# Get Apple Music Chrome app instance
process="chromium.instance$(ps x | rg "kjbdgfilnfhdoflbpgamdcdgpehopbep" | rg -v "rg" | xargs | cut -f 1 -d ' ')"

# Present choices using rofi and store the selection
selected=$(printf '%s\n' "${choices[@]}" | rofi -dmenu -i)

case "$selected" in
    "⏯️ lofibar")
        playerctl play-pause --player lofibar
        ;;
    "mpd")
        mpc toggle
        ;;
    "⏯️ 🍎 music")
        playerctl play-pause --player "${process}"
        dunstify "⏯️ $(playerctl metadata -f '{{title}} - {{artist}}')"
        ;;
    "⏭️ next")
        playerctl next --player "${process}"
        dunstify "$(playerctl metadata -f '{{title}} - {{artist}}')"
        ;;
    "🔅 set brightness")
        current=$(win-switch-rs brightness | cut -d ":" -f 2)
        win-switch-rs brightness -v $(rofi -dmenu -mesg $current) && \
            dunstify "🔅 $(win-switch-rs brightness)"
        ;;
    *)
        # If no valid selection was made, exit
        exit 1
        ;;
esac
