#!/bin/bash

set -e

echo "Copying psql files to the machine..."

psql_backup_path="${YANDEXDISK_DIR}/configs/psql"
cp ${psql_backup_path}/.psqlrc ~/
