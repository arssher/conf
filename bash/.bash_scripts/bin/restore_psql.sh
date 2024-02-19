#!/bin/bash

set -e

echo "Copying psql files to the machine..."

psql_backup_path="${CONFPATH}/psql"
cp ${psql_backup_path}/.psqlrc ~/
cp ${psql_backup_path}/.psql_history ~/
