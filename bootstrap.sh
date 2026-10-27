#!/bin/bash

set -e

sudo apt-get update

# lay down the dotfiles
sh -c "$(curl -fsLS get.chezmoi.io)" -- -b ~/.local/bin
chezmoi init --ssh --apply arssher/conf
source ~/.bashrc

curl -fsSL https://claude.ai/install.sh | bash

# Secrets, history and work-specific scripts are not in the repo. Once whatever
# syncs the private directory is up and CONFPATH points at it, its own README
# has the restore steps; the scripts are in $CONFPATH/bin.

sudo apt-get install emacs python3-pip python3-venv

# install manually chrome, vscode, slack (from deb package)

# vscode's Settings Sync, if signed in, must leave out settings and keybindings:
# chezmoi applies both now and sync writes the same files, so with both on they
# overwrite each other. Extensions and the rest keep syncing. It is machine
# state with no settings.json key and no command-line form -- `code --sync off`
# lasts only the launch it is given to -- so by hand, once per machine:
#   palette > "Settings Sync: Configure..." > untick Settings and Keyboard Shortcuts
# See vscode/readme.txt, which also has how to check.

# first do things enough for the minimal remote box
sudo apt-get -y install vim git checkinstall locate \
     openssh-server \
     rsync fio sysstat hdparm hwinfo lshw hping3 \
     xsel curl
# zsh, as on mac: .zshenv and .zshrc are managed here, and .sh_env gives it and
# bash the same environment, so either can be the one in use. With the two
# plugins .zshrc looks for, as bootstrap_mac.sh has them: autosuggestions greys
# in the rest of a command from history, syntax-highlighting reddens one that
# does not resolve. Debian puts both under /usr/share, the last path .zshrc tries.
sudo apt-get -y install zsh zsh-autosuggestions zsh-syntax-highlighting
# And the login shell, as on mac. Asks for the password; terminals opened
# afterwards start zsh, while $SHELL itself changes at the next login. bash
# stays installed and in step through .sh_env, for scripts and `bash' by hand.
chsh -s /bin/zsh
# fzf, for the zsh widgets .zshrc sets up from it: ^R over history, ^T over
# file paths, M-c over directories. Bookworm's 0.38 has no `fzf --zsh', so
# .zshrc falls back to the scripts the package leaves in
# /usr/share/doc/fzf/examples; trixie's 0.60 has the flag.
sudo apt-get -y install fzf

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

# Scanned documents: ocrmypdf adds a searchable text layer to a scan without
# touching the image, so the page still looks like a scan and the text is
# selectable. The same set as bootstrap_mac.sh, which has the longer notes on
# accuracy and unpaper. ghostscript and tesseract-ocr come in as dependencies;
# unpaper (deskewing), pngquant and jbig2 (compression) as recommends, which
# apt installs by default.
#
# tesseract-ocr carries English and the orientation data only; every other
# language is its own tesseract-ocr-<lang> package, the counterpart of brew's
# tesseract-lang -- here Russian, French and Dutch. Usage:
#
#   ocrmypdf -l rus+eng in.pdf out.pdf
#
# Give -l only the languages the document actually has: rus on a pure Latin
# page returns Cyrillic look-alikes. --deskew --clean are worth adding.
# --clean will not cut the border away, because ocrmypdf hands unpaper
# --layout none (Debian's 16.7 too, in ocrmypdf/_exec/unpaper.py); to get it
# back, override the defaults wholesale:
#
#   ocrmypdf --clean-final --unpaper-args '--layout single --mask-scan-size 100'
#
# unpaper and qpdf are named so that apt marks them manual, and autoremove
# leaves them behind if ocrmypdf ever goes. qpdf is the page surgery:
#
#   qpdf in.pdf --pages . 2-4 -- out.pdf    # also 1,3,5 / z-1 / 1-6:even
#   qpdf in.pdf --pages . 1-6,x3 -- out.pdf # x excludes, and must be comma
#                                           # joined inside the one range
#   qpdf --empty --pages a.pdf b.pdf -- merged.pdf
#   qpdf --split-pages in.pdf out.pdf       # out-1.pdf, out-2.pdf, ...
#   qpdf in.pdf --rotate=+90:1 out.pdf
#
# poppler-utils is for pdftotext, which tells whether a file already carries a
# text layer -- the default mode errors out when it does, and the recovery is
# picking between --skip-text, --redo-ocr and --force-ocr. It also brings
# pdfinfo, pdfimages and pdfseparate. pdfgrep greps a directory of PDFs and
# reports the page each hit is on. scantailor is not in trixie; see the mac
# notes for why it is skipped there too.
sudo apt-get -y install ocrmypdf tesseract-ocr-rus tesseract-ocr-fra tesseract-ocr-nld \
     unpaper qpdf poppler-utils pdfgrep

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

# ghostty. Debian has no package and upstream no official linux build; the
# snap is built with ghostty's own scripts and is classic -- unconfined, so it
# reads ~/.config/ghostty and starts the real shell, as a terminal must.
# Updates itself; `snap refresh --hold ghostty' pins it if one breaks.
sudo snap install ghostty --classic

flatpak install flathub org.telegram.desktop

flatpak install --user flathub org.keepassxc.KeePassXC

# Syncthing, from upstream's apt repo rather than Debian's: trixie's package
# stays on 1.29 for its whole life, while upstream -- and the mac's brew -- are
# on 2.x, with monthly releases that arrive through the usual apt upgrade. The
# snap is abandoned at 1.4.
#
# Check the key rather than trust the download: gpg --show-keys should print
#   FBA2 E162 F2F4 4657 B38F  0309 E566 5F9B D597 0C47
# the 2024 release key, certified by the 2014 one, D26E6ED000654A3E, which
# syncthing.net/security still quotes. Its signatures are SHA-256; the 2014
# key's SHA-1 binding is what trixie's sqv rejects, since 2026. The pin keeps
# upstream's package ahead of Debian's whatever the versions.
#
# The package's user unit is enabled through chezmoi -- the symlink in
# ~/.config/systemd/user/default.target.wants is managed -- so it runs from the
# next login, or now with `systemctl --user start syncthing'. GUI on
# http://127.0.0.1:8384.
sudo mkdir -p /etc/apt/keyrings
sudo curl -fsSL -o /etc/apt/keyrings/syncthing-archive-keyring.gpg https://syncthing.net/release-key.gpg
gpg --show-keys /etc/apt/keyrings/syncthing-archive-keyring.gpg
echo "deb [signed-by=/etc/apt/keyrings/syncthing-archive-keyring.gpg] https://apt.syncthing.net/ syncthing stable-v2" \
    | sudo tee /etc/apt/sources.list.d/syncthing.list
printf 'Package: *\nPin: origin apt.syncthing.net\nPin-Priority: 990\n' \
    | sudo tee /etc/apt/preferences.d/syncthing.pref
sudo apt-get update
sudo apt-get -y install syncthing

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

# xremap, the ergoemacs gestures outside emacs: a system service as its own
# user, plus a bridge as you for window matching. The script builds it (x11
# needs libx11-dev), creates the users and groups, and installs the root-owned
# copies of ~/.config/xremap; see the unit there. Needs chezmoi applied first,
# and a relogin before the bridge can reach the daemon.
sudo apt-get -y install libx11-dev
~/.bash_scripts/bin/install_xremap.sh

# fast app moving between monitors with move-to-next-monitor
# don't use it currently
# sudo apt-get -y install xdotool wmctrl

# md-viewer is decent markdown viewer
