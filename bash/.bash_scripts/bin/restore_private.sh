#!/bin/bash

set -e

echo "Copying private files to the machine..."

backup_path="${CONFPATH}/private"

if [ ! -d "${backup_path}" ]; then
    echo "skipping restore of private files as ${backup_path} doesn't exist"
    exit
fi

mkdir -p ~/.ssh
shopt  -s dotglob
cp -rp ${backup_path}/* ~/

cp ${backup_path}/.psql_history ~/

sudo cp  ${CONFPATH}/etc/NetworkManager/system-connections/* /etc/NetworkManager/system-connections/
