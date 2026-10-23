This repo is a chezmoi source directory. chezmoi keeps $HOME in sync with it.

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

What init does, in order:

  1. clones the repo into the source dir, ~/.local/share/chezmoi. If no repo
     is named it initialises an empty git repo there instead. If the source
     dir is already a git repo it leaves it alone.
  2. renders .chezmoi.toml.tmpl into ~/.config/chezmoi/chezmoi.toml.
  3. runs chezmoi apply -- but only if --apply was given.

So plain `init` touches nothing in $HOME: init, then diff, then apply is the
cautious order, and the diff is worth reading the first time.

--ssh makes it guess git@github.com:... rather than an https URL, which is
what you want if you also push from the machine.


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

  docs/, bootstrap/ and vscode/ are likewise tracked but not applied. vscode/
  holds only notes now: its keybindings and, on linux, its settings.json are
  applied from private_dot_config/Code/User/, see Shortcuts below.


Shell startup files:

  Two shells, one environment. ~/.sh_env holds everything both need -- PATH,
  CONFPATH, locale, EDITOR, the core limit, nvm/cargo/deno -- and three files
  source it so that no shell misses it:

    ~/.zshenv    every zsh: login or not, interactive or not, scripts too
    ~/.bashrc    every bash, from the top, above the interactivity guard
    ~/.profile   a login sh/dash, plus a login bash, which then sources .bashrc

  .sh_env must therefore stay POSIX and silent -- dash reads it, and output
  from it corrupts scp and rsync. It guards itself with an exported
  SH_ENV_LOADED so the environment loads once per session tree, not once per
  nested shell. To re-read it after an edit, run source_env: a function defined
  there, so both shells have it, which clears that guard and GLOBAL_VARS_LOADED
  and sources both files.

  The private ~/.global_vars is NOT sourced from .sh_env. It holds API tokens,
  and .sh_env is read by .profile, which the X11 session sources -- see
  .profile's header. It is sourced from the interactive halves of .bashrc and
  .zshrc instead, so a non-interactive shell does not get it.

  ~/.zprofile only calls sh_env_homebrew_path, defined in .sh_env's PATH
  section above the guard so nested shells have it too. It restores Homebrew
  priority after macOS's system login setup, without reloading toolchains or
  the private environment. PATH configuration stays in .sh_env.

  The interactive halves stay per-shell: .bashrc below its `case $- in *i*)`
  guard, and .zshrc, which is bindings only so far.

  macOS gotcha: /etc/zprofile runs path_helper in every login shell, and it
  builds PATH from /etc/paths, then /etc/paths.d, then inherited entries.
  /etc/paths.d/homebrew therefore puts Homebrew after /usr/bin. .zshenv runs
  before this; .zprofile runs after it and restores Homebrew's bin and sbin
  ahead of system tools. The function removes existing Homebrew entries first
  so repeated calls do not duplicate them. bootstrap_mac.sh includes
  `brew install python`; with this ordering, its python3 wins over Apple's.

  Installers write into these files: rustup appends to ~/.profile always and
  to ~/.zshenv whenever zsh is on PATH, and the claude installer has written
  ~/.zshrc before. Both are managed here, so such an append shows up as
  `chezmoi diff` drift and is reverted by the next apply -- hence
  --no-modify-path on the rustup line in bootstrap_mac.sh.


Shortcuts:

  Ergoemacs-inspired text manipulation shortcuts are supposed to be
  mostly kept in sync in the following places:
  - emacs
  - readline's ~/.inputrc
  - vscode, one keybindings.json per platform:
      linux  private_dot_config/Code/User/
      macos  "Library/Application Support/Code/User/"
    Each is .chezmoiignore'd on the other platform. settings.json is applied
    beside the linux one, the mac's not managed yet; vscode/readme.txt has the
    notes. The mac file is the shorter by far, because karabiner turns
    the gestures into standard macOS keys before vscode sees them: a real arrow
    serves the suggest widget, lists and notebooks at once, where linux needs
    an alt+i binding for each. Its header lists what karabiner covers, so what
    is missing from it is missing on purpose. Settings Sync must be kept off,
    or it and chezmoi overwrite each other.
  - ~/.zshrc
  - libedit's ~/.editrc
  - linux outside emacs through xremap, see below
  - macos whenever possible through karabiner remaps:

  private_dot_config/private_karabiner/karabiner.edn is the same rules in
  goku's EDN form. Kept for clarity: json has nowhere to put a comment, so
  the rationale sits inside the "description" fields karabiner shows in its
  UI, and the four bundle ids are repeated across twenty-six manipulators.
  NOT migrated -- nothing generates the json from it and .chezmoiignore keeps
  it out of $HOME, so it has to be updated by hand whenever the json changes
  or it goes stale and misleads. Its own header covers the goku syntax.

  xremap is karabiner's counterpart on linux, left alt in place of left cmd.
  Both send real keys -- arrows, Home, End, ctrl+arrows, BackSpace, Delete --
  rather than ESC+letter, so full-screen terminal programs such as Claude Code
  understand the gestures, not only readline. That is also why .inputrc and
  .zshrc bind the bare arrows to the same history search as M-i/M-k.

  It runs in two halves. A system service, as its own `xremap` user in the
  input group, holds the keyboards; a user unit, xremap-bridge, tells it over
  a socket which window has focus, so that emacs can be left alone. Nothing
  you run gets the input group. The daemon reads root-owned copies in /etc,
  never the ones chezmoi applies, so an edit to private_dot_config/xremap/
  takes two steps:
    chezmoi apply          prints "/etc is behind" -- it never runs sudo
    install_xremap.sh      copies into /etc under sudo, restarts the daemon
  The unit's header has the reasoning; install_xremap.sh pins the version.

Codex status line:

  private_dot_codex/modify_private_config.toml manages only tui.status_line in
  ~/.codex/config.toml through chezmoi's modify-template support. Model, plugin,
  MCP and project settings are read from the live file and retained, rather
  than copied into this public repo. Auth files and session state stay local.

  The footer shows hostname, model, context size, git branch, session tokens,
  context usage, five-hour/weekly quota remaining, and directory. /statusline
  changes the live selection; edit the list in the modify template to keep a
  new selection here. Do not re-add the whole Codex config.

  If the list already matches, the file is left byte for byte unchanged.
  Changing the list re-serializes TOML, preserving other settings' values but
  normalizing formatting and dropping comments. Apply just this target with:
    chezmoi diff ~/.codex/config.toml
    chezmoi apply ~/.codex/config.toml


Content kept outside this repo:

  A separate private directory holds what does not belong in git here.


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

Look through and manually run things from bootstrap.sh for the packages.

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

build and install emacs:
install_emacs.sh

ergoemacs keys outside emacs, after chezmoi apply (also in bootstrap.sh):
install_xremap.sh
then relogin, so the session joins xremap-<user> and the bridge can start

Important root configs:
/etc/NetworkManager/system-connections/
openvpn
fstab (deliberately not restored -- the UUIDs are machine-specific)
(/etc/xremap, its unit and the uinput udev rule need no backup:
install_xremap.sh recreates them from ~/.config/xremap)

Important home configs not saved anywhere:
the sync client's own config
firefox/chrome

The GNOME app launcher does not read .bashrc. To put a directory on the PATH
of graphical sessions use ~/.config/environment.d/, which the systemd user
session reads and which also works under wayland, where .profile is not
sourced at all. See private_dot_config/environment.d/.


How to play midi:
sudo apt-get install audicious fluid-soundfont-gm
wajig list-files fluid-soundfont-gm
And point midi plugin in audicious to listed .sf2 file

How to disable Alt+left mouse windows move
http://forums.odforce.net/topic/28501-linux-disable-alt-left-mouse-button/

------------------------------------------------------------
macos stuff

Done by hand, no cli for these:
- "Use Option as Meta key", for a terminal that has no config file to put it
  in.  Without it option composes characters and readline never sees \e.
  Ghostty needs no clicking: macos-option-as-alt is in its config.

Three finger drag is scriptable, and bootstrap_mac.sh carries the lines
commented out.  Not used: three fingers cannot mean both drag and swipe, and
the swipe is worth more.  Re-login required either way.

see
bootstrap_mac.sh
