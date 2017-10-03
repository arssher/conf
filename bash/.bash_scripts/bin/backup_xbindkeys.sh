#!/bin/bash

configs_backup_path="${CONFPATH}/xbindkeys"
echo "Copying keybindings to the machine..."s

mkdir -p "${configs_backup_path}"
cp ~/.xbindkeysrc "${configs_backup_path}"
