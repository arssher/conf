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

  Contains only .gdbinit file.


readline conf:

  Contains only .inputrc file with keybindings like in my emacs.

xbindkeys conf:

  Contains only .xbindkeysrc

Things done on fresh Debian install:
set up yandex disk, or point CONFPATH to dir with cloned repo.
cd conf
run ${CONFPATH}/bash/.bash_scripts/bin/bash_ydisk_to_machine.sh
Now you can run all restore*.sh scripts

sudo apt-get install vim git terminator chromium checkinstall locate openvpn linux-tools
sudo apt-get -y build-dep emacs24
build and install emacs
sudo apt-get install
flex bison libreadline-dev zlib1g-dev
sudo apt-get install docbook docbook-xml docbook-xsl fop libxml2-utils opensp xsltproc
global colordiff
python3-venv
tor
wajig
# additional pg deps
vim sudo strace build-essential git screen gdb \
libwww-perl perl perl-modules libperl-dev libipc-run-perl tcl-dev \
libedit-dev libssl-dev zlib1g-dev libpam0g-dev libxml2-dev \
krb5-multidev libldap2-dev python-dev bison flex xsltproc gettext \
libicu-dev libkrb5-dev libxslt-dev pkgconf libzstd-dev

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

How to play midi:
sudo apt-get install audicious fluid-soundfont-gm
wajig list-files fluid-soundfont-gm
And point midi plugin in audicious to listed .sf2 file


TODO:
* Deal with terminal -- saving pans and tabs, dir paths?

How to disable Alt+left mouse windows move
http://forums.odforce.net/topic/28501-linux-disable-alt-left-mouse-button/
