#!/bin/bash
function cleanDesktop {
  shopt -s nullglob
  for file in /Users/asimpson/Desktop/*
  do
    if [ "$file" == "/Users/asimpson/Desktop/drawer" -o "$file" == "/Users/asimpson/Desktop/skype-attachments" ];then
      echo "don't move these";
    else
      if [[ -n $(find "$file" -amin +60) ]]; then
        /usr/local/bin/tag -a inbox "$file"
        mv "$file" /Users/asimpson/Dropbox/misc/
      fi
    fi
  done
}

cleanDesktop
