#!/bin/bash

set -e

echo "Copying tmux files to the CONFPATH (${CONFPATH})..."

backup_path="${CONFPATH}/tmux"
mkdir -p ${backup_path}
cp ~/.tmux.conf ${backup_path}
