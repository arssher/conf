#!/bin/bash

set -e

echo "Copying readline config to the machine..."

bashfiles_backup_path="${CONFPATH}/readline"
cp -p ${bashfiles_backup_path}/.inputrc ~/
# to activate without session reload, run
# bind -f ~/.inputrc
