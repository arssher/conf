#!/bin/bash

set -e

backup_path="${CONFPATH}/psql"
echo "Copying psql files to backup at ${backup_path}..."
rm -rf ${backup_path}/.psqlrc
mkdir -p ${backup_path}
cp ~/.psqlrc ${backup_path}/.psqlrc
# note: psql history is saved in private
