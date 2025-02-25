#! /usr/bin/env nix-shell
#! nix-shell -i bash xdotool -p xdotool bash

# Store the choices in an array
choices=("lofibar" "mpd" "Play/Pause Apple Music")

# Present choices using rofi and store the selection
selected=$(printf '%s\n' "${choices[@]}" | rofi -dmenu -i -p "Media Control")

case "$selected" in
    "lofibar")
        eval $(xdotool getmouselocation --shell)
        ORIGINAL_X=$X
        ORIGINAL_Y=$Y
        # Your existing pause script logic
        ICON_X=2553
        ICON_Y=16
        PAUSE_X=2547
        PAUSE_Y=38

        xdotool mousemove $ICON_X $ICON_Y click 1
        sleep 0.5
        xdotool mousemove $PAUSE_X $PAUSE_Y click 1
        xdotool mousemove $ORIGINAL_X $ORIGINAL_Y
        ;;
    "mpd")
        mpc toggle
        ;;
    "Play/Pause Apple Music")
        # Show i3 scratchpad
        i3-msg 'scratchpad show'
        
        # Wait for window to appear
        sleep 0.2
        
        # Press spacebar to play/pause (Apple Music)
        xdotool key space
        
        # Wait a moment for the command to register
        sleep 0.2
        
        # Hide window back to scratchpad
        i3-msg 'move scratchpad'
        ;;
    *)
        # If no valid selection was made, exit
        exit 1
        ;;
esac
