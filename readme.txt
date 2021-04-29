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
sudo apt-get install git vim
set up yandex disk, or point CONFPATH to dir with cloned repo, e.g.
git clone https://github.com/arssher/conf.git
cd conf
and
./bootstrap.sh

Install Ubuntu fonts, or terminator & emacs will complain.
Configure caps lock and layout switchover (mate-keyboard-properties).

# building emacs
sudo apt-get install gnutls-dev checkinstall
build and install emacs:
install_emacs.sh

uncomment WaylandEnable=false in
/etc/gdm3/daemon.conf
(in wayland gnome layout switching stops working after a while, wow)

Important root configs:
fstab
network/interfaces
openvpn

Important home configs not saved here:
ydisk
firefox


How to play midi:
sudo apt-get install audicious fluid-soundfont-gm
wajig list-files fluid-soundfont-gm
And point midi plugin in audicious to listed .sf2 file

How to disable Alt+left mouse windows move
http://forums.odforce.net/topic/28501-linux-disable-alt-left-mouse-button/
