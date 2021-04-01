#!/bin/sh

url=$(xclip -o -selection clipboard)

mpv "${url}"
