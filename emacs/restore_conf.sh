#!/bin/bash

set -e
. ~/.bashrc

configs_backup_path="${YANDEXDISK_DIR}/configs/emacs"
cp "${configs_backup_path}/.emacs" ~/
cp -r "${configs_backup_path}/.emacs.d" ~/

