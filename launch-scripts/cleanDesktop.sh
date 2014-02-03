#!/bin/bash
function cleanDesktop {
  shopt -s nullglob
  for file in /Users/asimpson/Desktop/*
  do
    if [ "$file" == "/Users/asimpson/Desktop/skype-attachments" -o "$file" == "/Users/asimpson/Desktop/old-downloads"];then
      echo "don't move these";
    else
      if [[ -n $(find "$file" -amin +60) ]]; then
        /usr/local/bin/tag -a inbox "$file"
        cp -R "$file" /Users/asimpson/Dropbox/misc/
        rm -rf "$file"
      fi
    fi
  done
}

cleanDesktop
