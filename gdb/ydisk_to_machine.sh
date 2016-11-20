#!/bin/bash

set -e

echo "Copying gdb files to the machine..."

bashfiles_backup_path="${YANDEXDISK_DIR}/configs/gdb"
rm -rf ~/.gdb
cp -rp ${bashfiles_backup_path}/.gdb* ~/
