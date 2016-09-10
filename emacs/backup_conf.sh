#!/bin/bash

set -e
. ~/.bashrc

configs_backup_path="${YANDEXDISK_DIR}/configs/emacs"
cp ~/.emacs.d/init.el "${configs_backup_path}"
cp -r ~/.emacs.d/init.el "${configs_backup_path}"

