#!/bin/bash

set -e

echo "Copying bash files to the machine..."

bashfiles_backup_path="${YANDEXDISK_DIR}/configs/bash"
rm -rf ~/.bash_scripts
cp -rp ${bashfiles_backup_path}/.bash* ~/