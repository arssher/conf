#!/bin/bash

set -e

echo "Copying psql files to the yandex disk..."

backup_path="${CONFPATH}/psql"
rm -rf ${backup_path}/.psqlrc
mkdir -p ${backup_path}
cp ~/.psqlrc ${backup_path}/.psqlrc
