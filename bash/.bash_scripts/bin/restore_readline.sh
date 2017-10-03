#!/bin/bash

set -e

echo "Copying readline config to the machine..."

bashfiles_backup_path="${CONFPATH}/readline"
cp -p ${bashfiles_backup_path}/.inputrc ~/
bind -f ~/.inputrc
