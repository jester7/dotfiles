#!/bin/bash

# Check if the destination path is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <backup-destination-path>"
  exit 1
fi

# Set the backup destination path
DEST_PATH=$1

# Create a timestamp
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")

# Get the hostname
HOSTNAME=$(hostname)

# Construct the backup filename
BACKUP_FILE="$DEST_PATH/emacs_backup_${HOSTNAME}_${TIMESTAMP}.tar.gz"

# Create the backup
tar -czf "$BACKUP_FILE" -C "$HOME" .emacs.d

# Output the backup file location
echo "Backup created at: $BACKUP_FILE"
