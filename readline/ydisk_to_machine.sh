#!/bin/bash

set -e

echo "Copying readline config to the machine..."

bashfiles_backup_path="${YANDEXDISK_DIR}/configs/readline"
cp -p ${bashfiles_backup_path}/.inputrc ~/
