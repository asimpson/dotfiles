#!/bin/sh

selection=$(slop -f %w,%h,%x,%y)
width=$(echo "${selection}" | cut -d , -f 1)
height=$(echo "${selection}" | cut -d , -f 2)
top=$(echo "${selection}" | cut -d , -f 4)
left=$(echo "${selection}" | cut -d , -f 3)

cvlc --no-video-deco --no-embedded-video --screen-fps=20 --screen-top=$top --screen-left=$left --screen-width=$width --screen-height=$height screen:// &
