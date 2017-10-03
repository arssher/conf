#!/bin/bash

set -e

echo "Copying gdb files to the yandex disk..."

backup_path="${CONFPATH}/gdb"
rm -rf ${backup_path}/.gdb
cp -r ~/.gdb ${backup_path}
cp ~/.gdbinit ${backup_path}
