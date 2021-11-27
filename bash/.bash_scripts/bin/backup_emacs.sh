#!/bin/bash

set -e

# TODO: add diff ~/.emacs.d/init.el $YANDEXDISK_DIR/configs/emacs/.emacs.d/init.el check
configs_backup_path="${CONFPATH}/emacs/.emacs.d/"
echo "Copying emacs conf from machine to ${configs_backup_path}"
cp  ~/.emacs.d/init.el "${configs_backup_path}"
cp -r ~/.emacs.d/themes "${configs_backup_path}"
rm -rf "${configs_backup_path/static}/static"
cp -r ~/.emacs.d/static "${configs_backup_path}"
cp  ~/.gnus.el "${CONFPATH}/emacs"
cp  ~/.offlineimaprc "${CONFPATH}/emacs"
cp  ~/.offlineimap.py "${CONFPATH}/emacs"
cp  ~/.mbsyncrc "${CONFPATH}/emacs"
