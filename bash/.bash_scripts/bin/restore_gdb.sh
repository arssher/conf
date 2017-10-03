#!/bin/bash

set -e

echo "Copying gdb files to the machine..."

bashfiles_backup_path="${CONFPATH}/gdb"
rm -rf ~/.gdb
cp -rp ${bashfiles_backup_path}/.gdb* ~/
