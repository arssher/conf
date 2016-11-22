#!/bin/bash

set -e

echo "Copying readline config to the yandex disk..."

backup_path="${YANDEXDISK_DIR}/configs/readline"
cp ~/.inputrc ${backup_path}
