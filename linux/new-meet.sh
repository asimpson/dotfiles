#! /usr/bin/env nix-shell
#! nix-shell -i python3 xdotool dunst -p python3 xdotool dunst

from urllib.parse import urlparse, urlunparse
import subprocess
import time

def clean_url(url):
    parsed = urlparse(url)
    return urlunparse((parsed.scheme, parsed.netloc, parsed.path, '', '', ''))

def notify(message):
    subprocess.run(['notify-send', 'Meet Creator', message])

# Launch Chrome and wait
subprocess.Popen(['google-chrome-stable'])
time.sleep(2)

# Open new Meet
subprocess.run(['xdotool', 'key', 'ctrl+l'])
time.sleep(0.1)
subprocess.run(['xdotool', 'type', 'https://meet.google.com/new'])
subprocess.run(['xdotool', 'key', 'Return'])

# Wait for page load and copy URL
time.sleep(3)
subprocess.run(['xdotool', 'key', 'ctrl+l'])
time.sleep(0.1)
subprocess.run(['xdotool', 'key', 'ctrl+c'])

# Get and clean URL
url = subprocess.check_output(['clipster', '-o', '-n', '1']).decode('utf-8').strip()
clean = clean_url(url)

# Save cleaned URL
subprocess.run(['clipster'], input=clean.encode('utf-8'))

# Notify completion
subprocess.run(['dunstify', 'Meet Creator', 'Meet URL copied to clipboard'])