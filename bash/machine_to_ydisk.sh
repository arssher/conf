#!/bin/bash

set -e

echo "Copying bash files to the yandex disk..."

bashfiles_backup_path="${YANDEXDISK_DIR}/configs/bash"
rm -rf ${bashfiles_backup_path}/.bash_scripts
cp -r ~/.bash_scripts ${bashfiles_backup_path}
cp ~/.bashrc ${bashfiles_backup_path}
