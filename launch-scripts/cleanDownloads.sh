#!/bin/bash
function moveDownloads {
  mkdir -p /Users/asimpson/Desktop/old-downloads
  /usr/local/bin/tag --set important /Users/asimpson/Desktop/old-downloads
  shopt -s nullglob
  for file in /Users/asimpson/Downloads/*
  do
    if [ "$file" ];then
     mv "$file" /Users/asimpson/Desktop/old-downloads/ 
    fi
  done
}

moveDownloads
