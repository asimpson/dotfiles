#!/bin/bash
# Please create a folder called "vhost" at /Applications/MAMP/Library/
# Add " Include vhosts/* " to the end of the httpd.conf file located at /Applications/MAMP/conf/apache/httpd.conf

# Ask for site name
if [ "$1" = "create" ] || [ "$1" = "add" ]; then
  echo "Enter the site name (eg Test Site):";
  read projectName;

  # Ask for document root
  echo "Enter the document root (relative to 'htdocs'):";
  read documentRoot;

  # Ask for domain name
  echo "Enter local domain: (eg. local.com)";
  read domain;

  # Add vhost
  touch /Applications/MAMP/Library/vhosts/$documentRoot;

  echo "
  ServerName $domain
  DocumentRoot /Applications/MAMP/htdocs/$documentRoot
    Options FollowSymLinks
    Options Indexes FollowSymLinks MultiViews
  ErrorLog /Applications/MAMP/logs/$domain.error.log" >> /Applications/MAMP/Library/vhosts/$domain;

  echo "
  127.0.0.1 $domain" >> /etc/hosts;

  # Restart MAMP
  /Applications/MAMP/bin/apache2/bin/apachectl restart;

  echo "Visit $domain:port/ to view your site";
fi

if [ "$1" = "remove" ] || [ "$1" = "delete" ]; then
    echo "Enter the site name you wish to remove"
    read siteName;

    sed -i.bak "/$siteName/d" /etc/hosts;
    rm /Applications/MAMP/Library/vhosts/$siteName;
fi