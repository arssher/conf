#!/bin/bash

set -e

echo "Copying emacs conf to the machine..."
cp -r "${YANDEXDISK_DIR}/configs/emacs/.emacs.d/"* ~/.emacs.d/
cp -r "${YANDEXDISK_DIR}/configs/emacs/.gnus.el" ~/
