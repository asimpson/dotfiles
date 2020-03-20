#!/usr/bin/env bash

mbsync -a
pkill -2 -u $UID mu # send SIGINT
sleep 1
mu index -m ~/Mail
