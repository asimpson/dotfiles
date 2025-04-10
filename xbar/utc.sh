#!/usr/bin/env bash

echo "UTC $(date +"%H:%M") ǀ MTN $(TZ='America/Denver' date +"%I:%M%p") ǀ EU $(TZ='Europe/Berlin' date +"%I:%M%p") ǀ UK $(TZ='Europe/London' date +"%I:%M%p")"
