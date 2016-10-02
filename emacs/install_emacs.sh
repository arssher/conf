#!/bin/bash

# installs emacs 24.5 on Ubuntu 14.04. Source repos must be enabled!

set -e

sudo apt-get update
sudo apt-get -y build-dep emacs24

wget -P ~/Downloads/ http://ftp.gnu.org/gnu/emacs/emacs-24.5.tar.gz
cd ~/Downloads

tar -xf emacs-24.5.tar.gz 
rm emacs-24.5.tar.gz
cd emacs-24.5 && ./configure && make -j4 && sudo checkinstall --pkgversion 24.5 --pkgname emacs24-5
