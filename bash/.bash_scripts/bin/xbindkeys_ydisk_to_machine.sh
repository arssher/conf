#!/bin/bash

configs_backup_path="${YANDEXDISK_DIR}/configs/xbindkeys"
echo "Copying keybindings to the machine..."
echo "ydir=${YANDEXDISK_DIR}"

cp "${configs_backup_path}/.xbindkeysrc" ~/
xbindkeys -p
