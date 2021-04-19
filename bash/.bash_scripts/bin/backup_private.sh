#!/bin/bash

set -e

echo "Copying private files to the CONFPATH (${CONFPATH})..."

backup_path="${CONFPATH}/private/"
mkdir -p ${backup_path}
cp ~/.persistent_history ${backup_path}
cp ~/.global_vars ${backup_path}
mkdir -p "${backup_path}/.ssh"
cp ~/.ssh/config "${backup_path}/.ssh/"
