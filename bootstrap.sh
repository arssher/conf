#!/bin/bash

set -e

sudo apt-get update
# restore_xbindkeys needs it
sudo apt-get install xbindkeys

# restore conf
export CONFPATH=$(pwd)
bash/.bash_scripts/bin/restore_all.sh
source ~/.bashrc

sudo apt-get install emacs python3-pip python3-venv

# install manually chrome, vscode, slack (from deb package)

# first do things enough for the minimal remote box
sudo apt-get -y install vim git checkinstall locate \
     openssh-server \
     rsync fio sysstat hdparm hwinfo lshw hping3 \
     xsel curl
sudo apt-get -y install build-essential gdb clang mold
# lang server stuff
sudo apt-get -y install clangd bear
# pg stuff
sudo apt-get -y install flex bison libreadline-dev zlib1g-dev \
     docbook docbook-xml docbook-xsl fop libxml2-utils opensp xsltproc \
     libwww-perl perl perl-modules libperl-dev libipc-run-perl tcl-dev \
     libedit-dev libssl-dev zlib1g-dev libpam0g-dev libxml2-dev \
     krb5-multidev libldap2-dev python3-dev bison flex xsltproc gettext \
     libicu-dev libkrb5-dev libxslt-dev pkgconf libzstd-dev \
     libdbi-perl libdbd-pg-perl \
     libcurl4-openssl-dev libseccomp-dev \
     protobuf-compiler libprotobuf-dev
# building tmux
sudo apt-get -y install libevent-dev
# building emacs
sudo apt-get -y install gnutls-dev checkinstall

# build and install emacs:
# install_emacs.sh

# various desktop stuff
sudo apt-get install mpv enca

# Approximately here starts desktop.

# install ansible
mkdir -p ~/venv
python3 -m venv ~/venv/ansible
source ~/venv/ansible/bin/activate
python3 -m pip install ansible

# some things are easier to configure with ansible
# partially obsolete, haven't checked it for a while
ansible-playbook --connection=local --inventory localhost, --ask-become-pass -e ansible_python_interpreter=/usr/bin/python3 bootstrap.yml

sudo apt-get -y install chromium  \
     openvpn terminator firmware-iwlwifi network-manager-openvpn-gnome \
     xclip gparted gnome-shell-extensions fonts-ubuntu

sudo apt-get install snapd flatpak

flatpak install flathub org.telegram.desktop

flatpak install --user flathub org.keepassxc.KeePassXC

# mutagen is for yt-dl
sudo apt-get -y install ffmpeg global colordiff python3-venv tor xcalib python3-mutagen unrar-free \
     vlc filezilla hexchat
# mu/mu4e
# need also mbsync, it disappeared
sudo apt-get install libgmime-3.0-dev libxapian-dev pass

# building usbmount
sudo apt-get -y install lockfile-progs
# themes
# arc theme is fine theme with light and dark variants, requires gnome icons
sudo apt-get -y install arc-theme gnome-icon-theme

echo fs.inotify.max_user_watches= 131070 | sudo tee -a /etc/sysctl.conf
echo fs.inotify.max_user_watches=524288  | sudo tee -a /etc/sysctl.conf
echo fs.inotify.max_user_instances=512 | sudo tee -a /etc/sysctl.conf
echo fs.file-max = 524288 | sudo tee -a /etc/sysctl.conf

# should also allow higher per process limit, putting to
# /etc/security/limits.conf
# *               soft    nofile          524288
# *               hard    nofile          524288
# and relogin. To check
ulimit -n

sudo sh -c "echo 'kernel.core_pattern = /tmp/core_{%E}_%p' > /etc/sysctl.d/60-core-pattern.conf"
sudo sysctl -p

# install rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
cargo install cork

# fast app moving between monitors with move-to-next-monitor
# don't use it currently
# sudo apt-get -y install xdotool wmctrl
