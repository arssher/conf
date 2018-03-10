#!/bin/bash

# installs emacs 25.1 on Ubuntu 14.04. Source repos must be enabled!

set -e

sudo apt-get update
sudo apt-get -y build-dep emacs24

wget -P ~/Downloads/ http://ftp.gnu.org/gnu/emacs/emacs-25s.3.tar.gz
cd ~/Downloads

tar -xf emacs-25.3.tar.gz
rm emacs-25.3.tar.gz
cd emacs-25.3 && ./configure && make -j4 && sudo checkinstall --pkgversion 25.3 --pkgname emacs
