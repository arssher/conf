#!/bin/bash

set -e

echo "Copying emacs conf from machine to Yandex Disk..."
configs_backup_path="${YANDEXDISK_DIR}/configs/emacs"
cp -r ~/.emacs.d/ "${configs_backup_path}"

