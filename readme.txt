"restore" scripts install ${CONFPATH} conf onto the machine, "backup"
scripts export machine's conf to ${CONFPATH}.
.bashrc here sets CONFPATH to ${YANDEXDISK_DIR}/configs.


Bash conf:

  Contains .bashrc and .bash_scripts folder. .bashrc reads file .global_vars, so
  you can create and specify machine-dependant global vars there. You can find
  example in .global_vars.example (it is also privately saved).

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
In bookworm as of 05.2023 there was also this issue, but it somehow seems to
got resolved after reboot or something. Nope, it is still there:
https://discourse.ubuntu.com/t/keyboard-layout-switching-shortcut-periodically-stops-working/7617
Also, setxkbmap doesn't work in wayland, and there is no map right alt to ctrl
in gnome tweaks.
Also, in wayland I had to replug monitor after restart to restore its settings.
Also, with wayland ctrl mouse scroll switches workspaces instead of e.g. zooming in
browsers, probably related to
https://gitlab.gnome.org/GNOME/gnome-shell/-/issues/1562
which can't be turned off, oh my.

Important root configs:
fstab (restoring not scripted)
/etc/NetworkManager/system-connections/
openvpn

Important home configs not saved here:
dropbox
firefox/chrome


How to play midi:
sudo apt-get install audicious fluid-soundfont-gm
wajig list-files fluid-soundfont-gm
And point midi plugin in audicious to listed .sf2 file

How to disable Alt+left mouse windows move
http://forums.odforce.net/topic/28501-linux-disable-alt-left-mouse-button/
