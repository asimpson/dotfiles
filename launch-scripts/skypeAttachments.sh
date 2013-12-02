#!/bin/bash
function deleteSkype {
  shopt -s nullglob
  for file in /Users/asimpson/Desktop/skype-attachments/*
  do
    if [ "$file" ];then
     mv "$file" /Users/asimpson/.Trash/ 
    fi
  done
}

deleteSkype
