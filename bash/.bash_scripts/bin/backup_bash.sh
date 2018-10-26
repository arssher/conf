#!/bin/bash

set -e

echo "Copying bash files to the yandex disk at ${CONFPATH}/bash..."

bashfiles_backup_path="${CONFPATH}/bash"
rm -rf ${bashfiles_backup_path}/.bash_scripts
cp -r ~/.bash_scripts ${bashfiles_backup_path}
cp ~/.bashrc ${bashfiles_backup_path}
