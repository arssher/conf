#!/bin/bash

set -e

echo "Copying emacs conf to the machine..."
configs_backup_path="${YANDEXDISK_DIR}/configs/emacs"
cp -r "${configs_backup_path}/.emacs.d" ~/

