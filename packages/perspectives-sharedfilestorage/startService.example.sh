#!/bin/sh
# Copy this file to startService.sh (gitignored) and fill in real Mega account credentials before running.
node "$(dirname "$0")/sharedfilestorage.js" --port=15680 --maxfiles=10 --maxkeys=100 --userid=YOUR_MEGA_ACCOUNT_EMAIL --password=YOUR_MEGA_ACCOUNT_PASSWORD --statefile=providedkeys.json
