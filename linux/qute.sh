#!/bin/sh

query=$(rofi -dpi 1 -dmenu -p "Search terms: ")
qutebrowser "${query}"
