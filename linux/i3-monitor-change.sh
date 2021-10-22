#!/bin/sh

export DISPLAY=:0
export XAUTHORITY=/home/adam/.Xauthority

reload() {
  dpi=$1
  # For applications supporting XSettings, `Xft/DPI' sets font scaling
  # (and sometimes interface scaling), `Gdk/WindowScalingFactor' sets
  # interface scaling with GTK 3, and `Gdk/UnscaledDPI' undo font scaling
  # for GTK 3 applications.
  > /home/adam/.xsettingsd cat <<EOF
Xft/DPI $(( ${dpi}*1024 ))
Gdk/WindowScalingFactor $(( ${dpi}/96 ))
Xcursor/size $(( ${dpi}/6 ))
Gdk/UnscaledDPI $(( ${dpi}*1024/(${dpi}/96) ))
EOF

  /run/current-system/sw/bin/pkill -HUP xsettingsd || /run/current-system/sw/bin/xsettingsd &

  # For QT applications.
  export QT_AUTO_SCREEN_SCALE_FACTOR=1

  # For miscellaneous applications.
  {
    echo Xft.dpi: "${dpi}"
    echo Xcursor.size: $(( ${dpi}/6 ))
  } | /run/current-system/sw/bin/xrdb -merge

  /run/current-system/sw/bin/i3-msg restart
  /run/current-system/sw/bin/pkill dunst
}

sleep 5

hdmiConnected=$(/run/current-system/sw/bin/xrandr | grep -qs "HDMI-2 connected ")
code=$?

if [ "${code}" -eq 0 ]; then
  /run/current-system/sw/bin/xrandr --output eDP-1 --off
  /run/current-system/sw/bin/xrandr --output HDMI-2 --auto
  reload 108
else
  /run/current-system/sw/bin/xrandr --output eDP-1 --auto
  reload 175
fi
