#!/bin/sh
# Copy this file to startService.sh (gitignored) and fill in real credentials before running.
node "$(dirname "$0")/src/selfregister.js" --port=5988 --rabbithost=localhost --rabbitport=15672 --admin=YOUR_RABBITMQ_ADMIN_USER --adminpassword=YOUR_RABBITMQ_ADMIN_PASSWORD --maxusers=100 --level=info
