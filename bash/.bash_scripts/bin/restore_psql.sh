#!/bin/bash

set -e

echo "Copying psql files to the machine..."

psql_backup_path="${CONFPATH}/psql"
cp ${psql_backup_path}/.psqlrc ~/
if [ -f ${CONFPATH}/private/.psql_history ]; then
    cp ${CONFPATH}/private/.psql_history ~/
else
    echo "skipping restoring .psql_history, not found"
fi
