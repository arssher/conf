#!/bin/bash

set -e

# restore conf
# this is the least invasive thing here
export CONFPATH=$(pwd)
bash/.bash_scripts/bin/restore_all.sh
source ~/.bashrc

sudo apt-get install python3-pip python3-venv

# install ansible
mkdir -p ~/venv
python3 -m virtualenv ~/venv/ansible
source ~/venv/ansible/bin/activate
python3 -m pip install ansible

# some things are easier to configure with ansible
ansible-playbook --connection=local --inventory localhost, --ask-become-pass -e ansible_python_interpreter=/usr/bin/python3 bootstrap.yml

sudo apt-get -y install vim git terminator chromium checkinstall locate \
     openvpn terminator openssh-server firmware-iwlwifi network-manager-openvpn-gnome \
     rsync xclip gparted fio sysstat hdparm hwinfo lshw
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
     libdbi-perl libdbd-pg-perl \
     libcurl4-openssl-dev libseccomp-dev

# mutagen is for yt-dl
sudo apt-get -y ffmpeg install global colordiff python3-venv tor xcalib python3-mutagen unrar-free \
     vlc filezilla hexchat
# mu/mu4e
# need also mbsync, it disappeared
sudo apt-get install libgmime-3.0-dev libxapian-dev pass

# fast app moving between monitors with move-to-next-monitor
# don't use it currently
# sudo apt-get -y install xdotool wmctrl

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

sudo sysctl -w kernel.core_pattern=/tmp/core_{%E}_%p

cargo install cork
