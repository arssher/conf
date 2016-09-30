#!/bin/bash

#This folder contains linux mint custom keybindings.
#See http://10pm.ca/blog/p/export-and-import-keyboard-shortcuts-on-linux-mint-17-and-17-1/ for details.

configs_backup_path="${YANDEXDISK_DIR}/configs/mint_shortcuts"
echo "Copying keybindings to the machine..."
echo "ydir=${YANDEXDISK_DIR}, iname=${IDEA_BIN} and pname=${PYCHARM_BIN}, cname=${CLION_BIN}"

# substitute ${varname} in dconf-settings-template.conf with env variables
echo "${configs_backup_path}/dconf-settings.conf"
perl -p -e 's/\$\{([^}]+)\}/defined $ENV{$1} ? $ENV{$1} : $&/eg; s/\$\{([^}]+)\}//eg' "${configs_backup_path}/dconf-settings-template.conf" > "${configs_backup_path}/dconf-settings.conf"

dconf load /org/mate/desktop/keybindings/ < "${configs_backup_path}/dconf-settings.conf"
