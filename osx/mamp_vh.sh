#!/bin/bash
# Add " Include vhosts/httpd-vhosts.conf " to the end of the httpd.conf file located at /Applications/MAMP/conf/apache/httpd.conf

mkdir -p /Applications/MAMP/Library/vhosts;
mkdir -p /Applications/MAMP/Library/vhosts/domains;

if [ "$1" = "create" ] || [ "$1" = "add" ]; then
 # Ask for document root
  echo "Enter the document root (relative to 'htdocs'):";
  read documentRoot;

  # Ask for domain name
  echo "Enter local domain: (eg. local.com):";
  read domain;

   # Ask for domain name
  echo "MAMP Port Nubmer";
  read port;

  # Add vhost
  touch /Applications/MAMP/Library/vhosts/domains/$domain;

  echo "<VirtualHost *:$port>
    DocumentRoot "/Applications/MAMP/htdocs/$documentRoot"
    ServerName $domain
    <Directory "/Applications/MAMP/htdocs/$documentRoot">
        Options All
        AllowOverride All
        Order allow,deny
        Allow from all
    </Directory>
</VirtualHost>" >> /Applications/MAMP/Library/vhosts/domains/$domain;

  echo "127.0.0.1 $domain" >> /etc/hosts;

  # Restart MAMP
  /Applications/MAMP/bin/apache2/bin/apachectl restart;

  echo "Visit $domain:port to view your site";
fi

if [ "$1" = "remove" ] || [ "$1" = "delete" ]; then
    echo "Here are the current custom local domains:"
    for file in /Applications/MAMP/Library/vhosts/domains/*
    do
      if [ -f "$file" ];then
       echo ${file##/*/}
      fi
    done
    echo "Enter the site name you wish to remove:"
    read siteName;

    sed -i.bak "/$siteName/d" /etc/hosts;
    rm /Applications/MAMP/Library/vhosts/domains/$siteName;
fi