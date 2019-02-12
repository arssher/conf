#!/bin/bash

set -e

sudo apt-get install -y libevent-dev xclip

tmux_ver=2.8
wget -P ~ "https://github.com/tmux/tmux/archive/${tmux_ver}.tar.gz"
tar -xvzf "${HOME}/${tmux_ver}.tar.gz"
cd "${HOME}/tmux-${tmux_ver}" && sh autogen.sh && ./configure && make -j4
sudo checkinstall --nodoc --pkgversion ${tmux_ver} --pkgname tmux
sudo dpkg -i "${HOME}/tmux-${tmux_ver}/tmux_${tmux_ver}-1_amd64.deb"
