"restore" scripts install ${CONFPATH} conf onto the machine, "backup"
scripts export machine's conf to ${CONFPATH}.
.bashrc here sets CONFPATH to ${YANDEXDISK_DIR}/configs.


Bash conf:

  Contains .bashrc and .bash_scripts folder. .bashrc reads file .global_vars, so
  you can create and specify machine-dependant global vars there. You can find
  example in .global_vars.example.

  .bash_scripts contains bin folder, I put executable scripts there. The
  directory will be added to the PATH in .bashrc (subdirectories will not be
  added).


Emacs conf:

  Only init.el file and two dirs -- static_packages and themes -- are backup'ed.
  The rest is the ELPA's business.

gdb conf:

  Contains .gdbinit file and .gdb dir.


readline conf:

  Contains only .inputrc file with keybindings like in my emacs.

xbindkeys conf:

  That's what currently is used for keybindings.
  Contains only .xbindkeysrc

terminator conf:

  Contains only terminator conf

Things done on fresh Debian install:
sudo apt-get update
sudo apt-get install git vim xbindkeys terminator
set up yandex disk, or point CONFPATH to dir with cloned repo, e.g.
git clone https://github.com/arssher/conf.git
export CONFPATH=$HOME/conf
cd conf
run ${CONFPATH}/bash/.bash_scripts/bin/restore_all.sh
Install Ubuntu fonts, or terminator & emacs will complain.
Configure caps lock and layout switchover (mate-keyboard-properties).

sudo apt-get install vim git terminator chromium checkinstall locate openvpn linux-tools

# building emacs
sudo apt-get install gnutls-dev checkinstall
build and install emacs:
install_emacs.sh

sudo apt-get install flex bison libreadline-dev zlib1g-dev
sudo apt-get install docbook docbook-xml docbook-xsl fop libxml2-utils opensp xsltproc
sudo apt-get install global colordiff python3-venv tor wajig
# fast app moving between monitors with move-to-next-monitor
sudo apt-get install xdotool wmctl
# additional pg deps
vim sudo strace build-essential git gdb \
libwww-perl perl perl-modules libperl-dev libipc-run-perl tcl-dev \
libedit-dev libssl-dev zlib1g-dev libpam0g-dev libxml2-dev \
krb5-multidev libldap2-dev python-dev bison flex xsltproc gettext \
libicu-dev libkrb5-dev libxslt-dev pkgconf libzstd-dev \
libdbi-perl libdbd-pg-perl \
resolvconf xcalib
# building tmux
sudo apt-get install libevent-dev
# building usbmount
sudo apt-get install lockfile-progs
# themes
# arc theme is fine theme with light and dark variants, requires gnome icons
sudo apt-get install arc-theme gnome-icon-theme

sudo pip install virtualenvwrapper

Important root configs:
fstab
network/interfaces
openvpn

Important home configs not saved here:
ssh
fonts, appearance -- can be dumped with dconf
ydisk
firefox

How to play midi:
sudo apt-get install audicious fluid-soundfont-gm
wajig list-files fluid-soundfont-gm
And point midi plugin in audicious to listed .sf2 file

How to disable Alt+left mouse windows move
http://forums.odforce.net/topic/28501-linux-disable-alt-left-mouse-button/
