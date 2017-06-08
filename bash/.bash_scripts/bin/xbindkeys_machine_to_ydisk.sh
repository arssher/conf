#!/bin/bash

configs_backup_path="${YANDEXDISK_DIR}/configs/xbindkeys"
echo "Copying keybindings to the machine..."
echo "ydir=${YANDEXDISK_DIR}"

mkdir -p "${configs_backup_path}"
cp ~/.xbindkeysrc "${configs_backup_path}"
