#!/bin/bash

set -e

# bashrc is not yet loaded, so we need to set YANDEXDISK_DIR separately
export YANDEXDISK_DIR=`cat ~/.config/yandex-disk/config.cfg | grep "dir=" | sed 's/dir=\"\(.*\)\"/\1/'`

bash "${CONFPATH}/bash/ydisk_to_machine.sh"
bash "${CONFPATH}/mint_shortcuts/ydisk_to_machine.sh"
bash "${CONFPATH}/emacs/ydisk_to_machine.sh"
echo "Now run source ~/.bashrc to get changes"
