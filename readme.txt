"to machine" scripts install yandex disk conf onto the machine, "to disk"
scripts export machine's conf to yandex_disk


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

  Contains only .gdbinit file.


readline conf:

  Contains only .inputrc file with keybindings like in my emacs.

xbindkeys conf:

  Contains only .xbindkeysrc

Things done on fresh Debian install:
set up yandex disk, or (currently) point YANDEXDISK_DIR to dir with 'configs'
dir
run configs/bash/.bash_scripts/bin/bash_ydisk_to_machine.sh
Now you can run all ydisk_to_machine.sh scripts

sudo apt-get install vim git terminator chromium checkinstall locate openvpn linux-tools
sudo apt-get -y build-dep emacs24
build and install emacs
sudo apt-get install
flex bison libreadline-dev zlib1g-dev jadetex
global colordiff

Important root configs:
fstab
network/interfaces
openvpn

Important home configs not saved here:
ssh
fonts
ydisk
terminator
firefox

TODO:
* Deal with terminal -- saving pans and tabs, dir paths?
