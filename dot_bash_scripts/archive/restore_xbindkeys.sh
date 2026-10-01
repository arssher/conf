#!/bin/bash

if [ -x "$(command -v xbindkeys)" ]; then
    configs_backup_path="${CONFPATH}/xbindkeys"
    echo "Copying keybindings to the machine..."

    cp "${configs_backup_path}/.xbindkeysrc" ~/
    xbindkeys -p
fi;
