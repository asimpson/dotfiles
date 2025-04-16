#!/usr/bin/env bash

echo "UTC $(TZ='UTC' date +"%H:%M")"
echo "---" 
echo "MTN $(TZ='America/Denver' date +"%I:%M%p")"
echo "EU $(TZ='Europe/Berlin' date +"%I:%M%p")"
echo "UK $(TZ='Europe/London' date +"%I:%M%p")"
