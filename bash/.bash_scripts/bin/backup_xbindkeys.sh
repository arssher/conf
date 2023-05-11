#!/bin/bash

configs_backup_path="${CONFPATH}/xbindkeys"
echo "Copying keybindings to ${configs_backup_path}..."

mkdir -p "${configs_backup_path}"
cp ~/.xbindkeysrc "${configs_backup_path}"
