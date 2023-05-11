#!/bin/bash

# installs emacs on Ubuntu/Debian. Source repos must be enabled!

set -e

sudo apt-get update
sudo apt-get -y build-dep emacs

ver=28.2
link="https://ftp.gnu.org/pub/gnu/emacs/emacs-${ver}.tar.gz"
wget -nc -P . $link

tar -xf emacs-$ver.tar.gz
rm emacs-$ver.tar.gz
cd emacs-$ver

numcores=`cat /proc/cpuinfo | awk '/^processor/{print $3}' | tail -1`

# system install (doesn't work because have no idea why)
# ./configure && make -j${numcores} && sudo checkinstall --pkgversion $ver --pkgname emacs

# user install
./configure --prefix ${HOME}/opt && make -j${numcores} && make install
# echo "export PATH=${HOME}/opt/bin:$PATH" >> ~/.global_vars

# one-time start to install packages
emacs -batch --eval '(message "Hi!")'
