#!/bin/bash

# installs emacs on Ubuntu 14.04. Source repos must be enabled!

set -e

sudo apt-get update
sudo apt-get -y build-dep emacs24

link=https://ftp.gnu.org/pub/gnu/emacs/emacs-26.2.tar.gz
ver=26.2
wget -P ~/Downloads/ $link
cd ~/Downloads

tar -xf emacs-$ver.tar.gz
rm emacs-$ver.tar.gz
cd emacs-$ver

numcores=`cat /proc/cpuinfo | awk '/^processor/{print $3}' | tail -1`

# system install
# ./configure && make -j${numcores} && sudo checkinstall --pkgversion $ver --pkgname emacs

# user install
./configure --prefix ${HOME}/opt && make -j${numcores} && make install
echo "export PATH=${HOME}/opt/bin:$PATH" >> ~/.global_vars
