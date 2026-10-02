This repo is a chezmoi source directory. chezmoi keeps $HOME in sync with it.
The repo is public, so nothing secret or work-specific belongs here.

Workflow:

  chezmoi init --ssh --apply arssher/conf   on a new machine
  chezmoi cd                                shell in the source dir
  chezmoi diff                              what would change in $HOME
  chezmoi apply                             source -> $HOME
  chezmoi re-add                            $HOME -> source
  chezmoi add ~/.newrc                      start managing a new file
  chezmoi update                            git pull, then apply

After chezmoi add, commit in the source dir as usual: chezmoi cd && git add
... && git commit && git push.


Source naming:

  dot_foo              -> ~/.foo
  executable_foo       -> +x on the target. Required: git records only one
                          exec bit and chezmoi does not infer the mode from it.
  private_dot_config   -> ~/.config at 0700. private_ strips group and world
                          bits whatever the umask. It applies to that entry
                          only, not recursively.

  Nested dotfiles need the prefix at every level, e.g.
  dot_gdb/vuvova-gdb-tools/dot_gitignore. Miss it and chezmoi treats the file
  as internal and silently never applies it.


Things that are easy to get wrong:

  .chezmoiignore matches TARGET paths, not source paths. The script archive is
  ".bash_scripts/archive", not "dot_bash_scripts/archive". Spelled the source
  way it matches nothing and the archive gets applied.

  A .tmpl suffix is load-bearing. Without it {{ include ... }} and
  {{ .chezmoi.destDir }} are not evaluated and the file quietly behaves as a
  literal string.

  chezmoi reads the filesystem, not git. Gitignored is not chezmoi-ignored, so
  anything sitting in the source dir gets applied unless .chezmoiignore says
  otherwise.

  File modes come from the umask, which is pinned in .chezmoi.toml.tmpl so that
  every machine produces the same 0755/0644 regardless of its own umask.

  chezmoi apply does NOT delete files it does not manage. Removing something
  from the source leaves the copy in $HOME alone.

  dot_bash_scripts/archive/ is tracked but .chezmoiignore'd: scripts parked for
  review, never applied. To put one back on PATH:
    chezmoi cd
    git mv dot_bash_scripts/archive/foo.sh dot_bash_scripts/bin/executable_foo.sh
  A few of them lack +x, so chmod when promoting.

  docs/, bootstrap/ and vscode/ are likewise tracked but not applied.


Content kept outside this repo:

  Secrets, ssh client config, shell and psql history, the desktop settings
  dump, and work-specific scripts live in a separate private directory, not in
  git. $CONFPATH in .bashrc points at it, and it has its own readme.

  The scripts that move things between there and the machine are in
  .bash_scripts/bin: restore_private.sh and backup_private.sh for $HOME,
  restore_root.sh and backup_root.sh for root-owned config, restore_de.sh and
  backup_de.sh for the desktop settings dump.

  restore_private.sh is fill-in-only: it never overwrites an existing file, and
  it sets modes itself rather than copying them from the source. Pushing the
  other way is backup_private.sh, which does overwrite, deliberately.

  chezmoi runs restore_private.sh once on first apply, via .chezmoiscripts. On
  a new machine $CONFPATH is usually not set yet at that point, so it no-ops
  and you run restore_private.sh by hand once the private directory is there.


Things done on fresh Debian install:

Add yourself to sudoers:
su -
usermod -aG sudo ars
groups ars
(reboot or relogin)

sudo apt-get update
sudo apt-get install git vim terminator rsync

Add
contrib non-free
sections to /etc/apt/sources.list.d/debian.sources
e.g. ubuntu fonts are there (fonts-ubuntu), without them terminator & emacs
will complain.

Install chezmoi and lay down the dotfiles:
sh -c "$(curl -fsLS get.chezmoi.io)" -- -b ~/.local/bin
chezmoi init --ssh --apply arssher/conf

Then set up whatever syncs the private directory, point CONFPATH at it, and
run restore_private.sh. Look through and manually run things from
bootstrap/bootstrap.sh for the packages.

Optionally sync home from old machine:
ssh-copy-id -f -i ~/.ssh/id_ed25519.pub ars@newmachine
(-f ignores already exists check and so doesn't need private key)
rsync -azvP /home/ars/ ars@newmachine:/home/ars/

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

# building emacs (obsolete, in 13 stock is good enough)
sudo apt-get install gnutls-dev checkinstall
build and install emacs:
install_emacs.sh

Important root configs (restore_root.sh handles the first two, see its notes):
/etc/NetworkManager/system-connections/
openvpn
fstab (deliberately not restored -- the UUIDs are machine-specific)

Important home configs not saved anywhere:
the sync client's own config
firefox/chrome

The GNOME app launcher does not read .bashrc. To put a directory on the PATH
of graphical sessions use ~/.config/environment.d/, which the systemd user
session reads and which also works under wayland, where .profile is not
sourced at all. See dot_config/environment.d/.


How to play midi:
sudo apt-get install audicious fluid-soundfont-gm
wajig list-files fluid-soundfont-gm
And point midi plugin in audicious to listed .sf2 file

How to disable Alt+left mouse windows move
http://forums.odforce.net/topic/28501-linux-disable-alt-left-mouse-button/
