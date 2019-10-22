#!/bin/bash

query=$(rofi -dpi 1 -lines 1 -dmenu -p 'Search docs');
zeal "$query";
