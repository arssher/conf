#!/bin/bash

configs_backup_path="${CONFPATH}/xbindkeys"
echo "Copying keybindings to the machine..."
echo "ydir=${YANDEXDISK_DIR}"

cp "${configs_backup_path}/.xbindkeysrc" ~/
xbindkeys -p
