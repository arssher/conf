#!/bin/bash

# macOS counterpart of bootstrap.sh. Same idea: not meant to be run top to
# bottom, read it and run the bits you need. The keyboard section is the
# interesting part.

set -e

# Command line tools first, everything below needs the compiler.
xcode-select --install

# homebrew. Installs to /opt/homebrew on apple silicon, /usr/local on intel;
# BREW below is that prefix, several steps need it spelled out.
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
BREW="$(brew --prefix)"

# lay down the dotfiles
brew install chezmoi
chezmoi init --ssh --apply arssher/conf

# rooster's perch
curl -fsSL https://claude.ai/install.sh | bash

# caps lock -> control, as on linux. Early, because everything below is typed.
# This is exactly what System Settings -> Keyboard -> Keyboard Shortcuts ->
# Modifier Keys writes: the line was captured by setting it in the panel once
# and reading the value back, and writing it again is idempotent. So a fresh
# machine needs no clicking for it.
#
# The numbers are HID usage codes on page 0x07: 0x700000039 is caps lock and
# 0x7000000E4 is RIGHT control -- which is what the panel picks, not 0xE0, the
# left one. Emacs is fine with that because ns-right-control-modifier defaults
# to 'left, meaning "whatever the other control is"; don't set it to anything
# else or caps lock stops acting as C- in emacs.
#
# The key is per keyboard. 0-0-0 identifies the internal apple keyboard, which
# reports vendor 0 and product 0; a plugged-in one gets a key of its own. To
# learn it, set the mapping in the panel for that keyboard and read it back:
#   defaults -currentHost read -g | grep -A6 modifiermapping
# If a write does not take effect right away, log out and back in.
#
# Once karabiner runs, this stops being what does the work. Karabiner seizes
# the physical keyboard and re-emits through its own virtual one (vendor 1452,
# product 592), so a mapping keyed to the internal keyboard's 0-0-0 is
# addressed to a device the system no longer gets events from, and caps lock
# goes back to being caps lock. The rule therefore also lives in
# karabiner.json, as a simple_modification. Keep this line anyway: it is what
# applies when karabiner is not in the path -- the login window, before the
# driver loads, karabiner disabled or removed -- and only one of the two can be
# active at a time, so they never fight.
defaults -currentHost write -g com.apple.keyboard.modifiermapping.0-0-0 \
  '({HIDKeyboardModifierMappingSrc=30064771129;HIDKeyboardModifierMappingDst=30064771300;})'

# Three finger drag: three fingers move a window or select text. The keys
# System Settings -> Accessibility -> Pointer Control -> Trackpad Options
# writes. Re-login required, and a half-applied state behaves erratically in
# between.
#
# Off by choice -- tried it, the three finger swipe is worth more. Kept here
# commented in case that changes; the swipes are the default so nothing needs
# undoing, beyond setting them back to 2 if drag had been on, which turning it
# off does not do by itself.
#
# Three fingers cannot mean both drag and swipe -- with both on the swipe wins,
# which looks just like the drag not working -- so the three finger swipes go
# off and spaces and mission control move to four fingers, already enabled.
# Dragging is the panel's checkbox, TrackpadThreeFingerDrag its style; the style
# alone does nothing. Two domains: the built-in trackpad and a Magic Trackpad
# keep their settings apart.
#
# macOS keeps com.apple.trackpad.* copies of all this in -currentHost -g too.
# Setting them looks unnecessary, but it is untested -- this machine has them
# set from when they were. Try them if the above does not take.
# for d in com.apple.AppleMultitouchTrackpad com.apple.driver.AppleBluetoothMultitouch.trackpad; do
#     defaults write "$d" Dragging -bool true
#     defaults write "$d" TrackpadThreeFingerDrag -bool true
#     defaults write "$d" TrackpadThreeFingerHorizSwipeGesture -int 0
#     defaults write "$d" TrackpadThreeFingerVertSwipeGesture -int 0
# done

# Secrets, history and work-specific scripts are not in the repo. Once whatever
# syncs the private directory is up and CONFPATH points at it:
#   restore_private.sh          $HOME bits
#   restore_root.sh             root-owned config, needs sudo
# restore_de.sh is linux desktop settings, nothing to restore here.

# visual.el asks for Ubuntu Mono, which macOS does not ship. Before emacs on
# purpose: the config falls back to the stock font when it is missing and only
# reconsiders per frame, so a font installed afterwards reaches the frames made
# from then on, not the ones already open.
brew install --cask font-ubuntu-mono

# Emacs: the ns port, to stay on the same version as linux. emacs-plus carries
# native-comp and the system-appearance patch. The cask is a prebuilt binary of
# that same formula built with default options -- which is all this machine
# wants -- so it skips a long source build and installs into /Applications
# itself.
#
# brew trust comes first: a third-party tap's formula or cask will not even
# load untrusted. Scoping it to the tap rather than one formula covers the cask
# and the next emacs-plus@NN too -- and lets that tap's code run unprompted.
brew tap d12frosted/emacs-plus
brew trust d12frosted/emacs-plus
brew install --cask emacs-plus-app
# Check what it actually ships:
#   emacs -Q --batch --eval '(message "%S" (native-comp-available-p))'
#
# Build from source instead when the prebuilt cannot give you something: the
# --with-* options (x11, xwidgets, debug, imagemagick, mailutils, dbus), --HEAD,
# or community patches from ~/.config/emacs-plus/build.yml, which are applied at
# build time -- the cask reads build.yml for icons only.
#   brew install emacs-plus     # not --with-native-comp: that option is gone,
#                               # @31 always builds it, and brew errors out on
#                               # an unknown option
# A formula does not install into /Applications, so for Spotlight and the Dock:
#   ln -s "$BREW/opt/emacs-plus@31/Emacs.app" /Applications/
#   ln -s "$BREW/opt/emacs-plus@31/Emacs Client.app" /Applications/
# Never both at once: each ships emacs/emacsclient in $BREW/bin and brew cannot
# declare a formula-to-cask conflict. The cask deliberately leaves an existing
# bin/emacs alone, so a stale link from the other one wins silently. Uninstall
# the one you are leaving before installing the other.
#
# emacsformacosx.com is an unrelated third option -- brew install --cask
# emacs-app -- a different build carrying none of the emacs-plus patches. The
# cask used to be named plain "emacs", which still resolves with a rename
# warning.

# start emacs, ensure it loads

# Keyboard. Karabiner does the left/right cmd split; goku turns compact EDN
# into its verbose json, optional.
brew install --cask karabiner-elements
# brew install yqrashawn/goku/goku

# basics
brew install vim git rsync curl wget htop jq
brew install ghostty

# bash as a program, not as the login shell: there is deliberately no chsh
# here, zsh stays the shell on mac. /etc/shells only makes brew's bash a
# permitted one, for a later chsh or for tools that consult the list.
#
# $(brew --prefix) rather than $BREW, even though line 15 sets it: this file is
# read and pasted from piecemeal, so a line that only works after line 15 has
# run is a line that appends "/bin/bash" to /etc/shells instead. Guarded as
# well, because that is how a duplicate got in there once already.
brew install bash
BASH_PATH="$(brew --prefix)/bin/bash"
grep -qxF "$BASH_PATH" /etc/shells || echo "$BASH_PATH" | sudo tee -a /etc/shells

# Build tooling. clang comes with the command line tools; gdb is not worth the
# fight on mac (codesigning, no apple silicon support), use lldb, which means
# .gdbinit and the postgres pretty printers in .gdb stay linux-only.
brew install llvm cmake pkgconf
# lang server stuff
brew install bear  # clangd ships with llvm above

# pg stuff. Note bison and flex: macOS has ancient ones in /usr/bin, so the
# brew versions must come first on PATH when building postgres.
brew install readline zlib icu4c openssl@3 gettext libxml2 libxslt \
     lz4 zstd krb5 openldap tcl-tk perl \
     bison flex docbook docbook-xsl fop
# export PATH="$BREW/opt/bison/bin:$BREW/opt/flex/bin:$PATH"

brew install tmux

# various desktop stuff
brew install --cask google-chrome visual-studio-code slack iterm2 \
     telegram keepassxc

# mu/mu4e, and pass for secrets
brew install mu isync pass

# media
brew install ffmpeg yt-dlp

# install rust. --no-modify-path because rustup otherwise appends
# `. "$HOME/.cargo/env"` to every rc file it finds -- ~/.profile always, and
# ~/.zshenv whenever zsh is on PATH, which on mac it always is. Both are
# chezmoi-managed, so the append would be reverted by the next apply and show
# up as drift in `chezmoi diff` until then. ~/.sh_env sources ~/.cargo/env
# itself, guarded, so nothing is lost.
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --no-modify-path
cargo install cork

# Limits. No /etc/sysctl.conf or security/limits.conf on mac; the shell limit
# is what matters in practice and macOS defaults it low.
ulimit -n
# ulimit -n 524288 in .profile, or a LaunchDaemon with limit maxfiles for a
# system wide one.

# Cores land in /cores and are off by default; sudo sysctl -w kern.coredump=1
# plus ulimit -c unlimited for a session. No core_pattern equivalent.
