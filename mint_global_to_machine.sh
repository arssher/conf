#!/bin/bash

set -e

# bashrc is not yet loaded, so we need to set YANDEXDISK_DIR separately
export YANDEXDISK_DIR=`cat ~/.config/yandex-disk/config.cfg | grep "dir=" | sed 's/dir=\"\(.*\)\"/\1/'`

bash "${YANDEXDISK_DIR}/configs/bash/ydisk_to_machine.sh"
source ~/.bashrc
bash "${YANDEXDISK_DIR}/configs/mint_shortcuts/ydisk_to_machine.sh"
bash "${YANDEXDISK_DIR}/configs/emacs/ydisk_to_machine.sh"
