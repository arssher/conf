#!/bin/bash

set -e

echo "Copying emacs conf to the machine..."
mkdir -p ~/.emacs.d
cp -r "${CONFPATH}/emacs/.emacs.d/"* ~/.emacs.d/
cp -r "${CONFPATH}/emacs/.gnus.el" ~/
