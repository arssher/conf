#!/bin/bash

set -e

echo "Copying tmux files to the machine..."

backup_path="${CONFPATH}/tmux"
cp ${backup_path}/.tmux.conf ~/.tmux.conf

# download tpm, if not yet
if [ ! -d ~/.tmux ]; then
    echo "downloading tpm"
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
else
    echo "tpm is already there"
fi
