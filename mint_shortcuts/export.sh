#!/bin/bash

#This folder contains linux mint custom keybindings.
#See http://10pm.ca/blog/p/export-and-import-keyboard-shortcuts-on-linux-mint-17-and-17-1/ for details.

. ~/.bashrc
configs_backup_path="${YANDEXDISK_DIR}/configs/mint_shortcuts"

dconf dump /org/mate/desktop/keybindings/ > "${configs_backup_path}/dconf-settings.conf"
