#!/bin/bash

set -e

echo "Copying psql files to the yandex disk..."

backup_path="${YANDEXDISK_DIR}/configs/psql"
rm -rf ${backup_path}/.psqlrc
cp ~/.psqlrc ${backup_path}
