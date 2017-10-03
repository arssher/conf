#!/bin/bash

set -e

# TODO: add diff ~/.emacs.d/init.el $YANDEXDISK_DIR/configs/emacs/.emacs.d/init.el check
echo "Copying emacs conf from machine to Yandex Disk..."
configs_backup_path="${CONFPATH}/emacs/.emacs.d/"
cp  ~/.emacs.d/init.el "${configs_backup_path}"
cp -r ~/.emacs.d/themes "${configs_backup_path}"
cp -r ~/.emacs.d/static "${configs_backup_path}"
cp  ~/.gnus.el "${CONFPATH}/emacs"
