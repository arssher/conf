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

brew install bash
echo "$BREW/bin/bash" | sudo tee -a /etc/shells

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

# install rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
cargo install cork

# Limits. No /etc/sysctl.conf or security/limits.conf on mac; the shell limit
# is what matters in practice and macOS defaults it low.
ulimit -n
# ulimit -n 524288 in .profile, or a LaunchDaemon with limit maxfiles for a
# system wide one.

# Cores land in /cores and are off by default; sudo sysctl -w kern.coredump=1
# plus ulimit -c unlimited for a session. No core_pattern equivalent.
