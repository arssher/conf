#!/bin/bash

set -e

# restore conf
# this is the least invasive thing here
export CONFPATH=$(pwd)
bash/.bash_scripts/bin/restore_all.sh
source ~/.bashrc

sudo apt-get install python3-pip

# install ansible
mkdir -p ~/venv
python3 -m virtualenv ~/venv/ansible
source ~/venv/ansible/bin/activate
python3 -m pip install ansible

# some things are easier to configure with ansible
ansible-playbook --connection=local --inventory localhost, --ask-become-pass -e ansible_python_interpreter=/usr/bin/python3 bootstrap.yml

sudo apt-get -y install vim git terminator chromium checkinstall locate \
     openvpn xbindkeys terminator openssh-server firmware-iwlwifi network-manager-openvpn-gnome
sudo apt-get -y install build-essential gdb clang
# lang server stuff
sudo apt-get -y install clangd bear
# pg stuff
sudo apt-get -y install flex bison libreadline-dev zlib1g-dev \
     docbook docbook-xml docbook-xsl fop libxml2-utils opensp xsltproc \
     libwww-perl perl perl-modules libperl-dev libipc-run-perl tcl-dev \
     libedit-dev libssl-dev zlib1g-dev libpam0g-dev libxml2-dev \
     krb5-multidev libldap2-dev python-dev bison flex xsltproc gettext \
     libicu-dev libkrb5-dev libxslt-dev pkgconf libzstd-dev \
     libdbi-perl libdbd-pg-perl

sudo apt-get -y install global colordiff python3-venv tor xcalib
# mu/mu4e
sudo apt-get install libgmime-3.0-dev libxapian-dev pass mbsync

# fast app moving between monitors with move-to-next-monitor
sudo apt-get -y install xdotool wmctrl

# building tmux
sudo apt-get -y install libevent-dev
# building usbmount
sudo apt-get -y install lockfile-progs
# themes
# arc theme is fine theme with light and dark variants, requires gnome icons
sudo apt-get -y install arc-theme gnome-icon-theme

# building emacs
sudo apt-get -y install gnutls-dev checkinstall
# build and install emacs:
install_emacs.sh
