#! /usr/bin/env nix-shell
#! nix-shell -i bash -p slop ffmpeg-full

selection=$(slop -f %w,%h,%x,%y)
width=$(echo "${selection}" | cut -d , -f 1)
height=$(echo "${selection}" | cut -d , -f 2)
top=$(echo "${selection}" | cut -d , -f 4)
left=$(echo "${selection}" | cut -d , -f 3)

ffplay -f x11grab -video_size ${width}x${height} -i :0.0+${left},${top} \
       -window_title "Screen Share" \
       -left 100 -top 100 &
