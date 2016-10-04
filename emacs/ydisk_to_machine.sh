#!/bin/bash

set -e

echo "Copying emacs conf to the machine..."
configs_backup_path=
cp -r "${YANDEXDISK_DIR}/configs/emacs/.emacs.d/*" ~/.emacs.d/

