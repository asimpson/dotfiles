#!/bin/bash

RED="\033[0;31m"
YELLOW="\033[33m"
REDBG="\033[0;41m"
WHITE="\033[1;37m"
NC="\033[0m"

if [ "$1" = "create" ] || [ "$1" = "add" ]; then
 # Ask for document root
  echo -e "${RED}Enter the document root (relative to '~/Projects'):${NC}";
  read documentRoot;

  # Ask for domain name
  echo -e "${RED}Enter local domain: (eg. local.com):${NC}";
  read domain;

  # Add vhost
  touch ~/Projects/vhosts/$domain;

  echo "<VirtualHost 0.0.0.0:80>
    DocumentRoot "/Users/asimpson/Projects/$documentRoot"
    ServerName $domain
    ServerAlias "$domain.*.xip.io"
</VirtualHost>" >> ~/Projects/vhosts/$domain;

  echo "127.0.0.1 $domain" >> /etc/hosts;

  # Restart MAMP
  sudo apachectl restart;

  echo -e "Finished. ${REDBG}${WHITE}$domain${NC} has been copied to your clipboard.";
  echo "$domain" | pbcopy;
fi

if [ "$1" = "remove" ] || [ "$1" = "delete" ]; then
    echo -e "${RED}Here are the current custom local domains:${NC}"
    for file in ~/Projects/vhosts/*
    do
      if [ -f "$file" ];then
       echo -e "${YELLOW}${file##/*/}${NC}"
      fi
    done
    echo -e "${RED}Enter the site name you wish to remove:${NC}"
    read siteName;

    sed -i.bak "/$siteName/d" /etc/hosts;
    rm ~/Projects/vhosts/$siteName;

    echo -e "${YELLOW}$siteName removed."
fi
