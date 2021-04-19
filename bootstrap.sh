#!/bin/bash

set -e

# restore conf
# this is the least invasive thing here
export CONFPATH=$(pwd)
bash/.bash_scripts/bin/restore_all.sh
source ~/.bashrc

sudo pip install virtualenvwrapper

# install ansible
mkvirtualenv ansible
python -m pip install ansible

sudo apt-get install vim git terminator chromium checkinstall locate openvpn linux-tools xbindkeys terminator
sudo apt-get install build-essential gdb
# pg stuff
sudo apt-get install flex bison libreadline-dev zlib1g-dev \
     docbook docbook-xml docbook-xsl fop libxml2-utils opensp xsltproc \
     libwww-perl perl perl-modules libperl-dev libipc-run-perl tcl-dev \
     libedit-dev libssl-dev zlib1g-dev libpam0g-dev libxml2-dev \
     krb5-multidev libldap2-dev python-dev bison flex xsltproc gettext \
     libicu-dev libkrb5-dev libxslt-dev pkgconf libzstd-dev \
     libdbi-perl libdbd-pg-perl \

sudo apt-get install global colordiff python3-venv tor wajig resolvconf xcalib

# fast app moving between monitors with move-to-next-monitor
sudo apt-get install xdotool wmctl

# building tmux
sudo apt-get install libevent-dev
# building usbmount
sudo apt-get install lockfile-progs
# themes
# arc theme is fine theme with light and dark variants, requires gnome icons
sudo apt-get install arc-theme gnome-icon-theme

# building emacs
sudo apt-get install gnutls-dev checkinstall
# build and install emacs:
install_emacs.sh
