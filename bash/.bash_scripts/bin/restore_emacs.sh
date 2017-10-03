#!/bin/bash

set -e

echo "Copying emacs conf to the machine..."
cp -r "${CONFPATH}/emacs/.emacs.d/"* ~/.emacs.d/
cp -r "${CONFPATH}/emacs/.gnus.el" ~/
