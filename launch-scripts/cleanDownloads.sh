#!/bin/bash
function moveDownloads {
  mkdir -p /Users/asimpson/Desktop/old-downloads
  /usr/local/bin/tag --set Red /Users/asimpson/Desktop/old-downloads
  for file in /Users/asimpson/Downloads/*
  do
    if [ "$file" ];then
     mv "$file" /Users/asimpson/Desktop/old-downloads/ 
    fi
  done
}

moveDownloads
