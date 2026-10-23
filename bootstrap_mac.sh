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

# Kill the big translucent letter that pops up mid-screen on switching input
# source -- ABC and Russian, here. It arrived with Sonoma and System Settings
# has no switch for it; this undocumented key is the only lever anyone has
# found. Takes effect per process as each one re-reads preferences, so log out
# and back in. Undo with `defaults delete`.
#
# UNVERIFIED past Sonoma: the write-ups for it are macOS 14 and this machine is
# 26.6.2, so it may simply do nothing now -- in which case delete the key
# rather than leave a dead pref behind. The other documented route is a feature
# flag under /Library/Preferences/FeatureFlags/Domain turning off the
# redesigned text cursor that the indicator belongs to, which is system-wide,
# needs a reboot, and gets reset by updates. Not worth it for this.
defaults write kCFPreferencesAnyApplication TSMLanguageIndicatorEnabled 0

# Ask for the password the moment the screen locks, rather than after a grace
# period. The default here was 300 seconds, which is longer than it sounds,
# because the setting covers more than the screen saver: System Settings ->
# Lock Screen words it "require password after screen saver begins or display
# is turned off", and putting the machine to sleep turns the display off. So
# with a delay, sleeping the laptop and waking it inside the window lands
# straight on the desktop with nothing asked -- and the walk-away case is
# worse, since the display's own idle timeout runs first and the two add up.
#
# FileVault does not cover this. It protects the disk at rest and asks at a
# cold boot; with hibernatemode 3 the machine keeps RAM powered over sleep, so
# a wake inside the window is just a wake.
#
# An explicit lock was always immediate regardless -- control+command+q, the
# Lock Screen menu item, or a press of touch id, which is what the power
# button does on a keyboard that has one. This only changes the paths that
# lock by themselves.
#
# Not sudo: sysadminctl authenticates with the user's own password, and `-`
# makes it prompt rather than taking it on the command line. So this one wants
# a terminal and cannot be pasted into a script that runs unattended.
#   sysadminctl -screenLock status      # to check
#   sysadminctl -screenLock 300 -password -   # to put the delay back
sysadminctl -screenLock immediate -password -

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
# syncs the private directory is up and CONFPATH points at it, its own README
# has the restore steps; the scripts are in $CONFPATH/bin. restore_de.sh there
# is linux desktop settings, nothing to restore here.

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

# Tree-sitter grammars, the same step install_emacs.sh does on linux. The cask
# already builds with tree-sitter -- check with
#   emacs -Q --batch --eval '(message "%S" (treesit-available-p))'
# -- but emacs bundles no grammar, only the recipes, so the compiling happens
# here, with the command line tools' cc and git. Markdown needs two, the block
# grammar and the inline one, which live in separate directories of a single
# repo. They land in ~/.emacs.d/tree-sitter/ as .dylib, a path that is not
# version-specific, so this survives the next emacs. About ten seconds.
#
# Not optional if you edit markdown: lang.el checks for both grammars before
# it points .md at markdown-ts-mode, so without them every .md quietly opens
# in markdown-mode instead, with none of the f6/f7 bindings.
#
# -Q because this must not depend on the init files loading cleanly, and the
# explicit path for the reason the lines above give -- a stale bin/emacs from
# the formula would otherwise answer here.
"$BREW/bin/emacs" -Q -batch --eval '(progn
  (require (quote markdown-ts-mode))
  (dolist (lang (quote (markdown markdown-inline)))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))'

# Keyboard. Karabiner does the left/right cmd split; goku turns compact EDN
# into its verbose json, optional.
brew install --cask karabiner-elements
# brew install yqrashawn/goku/goku

# basics
brew install vim git rsync curl wget htop jq
brew install ghostty

# Modern python3; ~/.sh_env puts Homebrew ahead of Apple's system Python.
brew install python

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

# The two zsh plugins .zshrc looks for: autosuggestions greys in the rest of a
# matching history line as you type, syntax-highlighting colours the line and
# reddens a command that does not resolve. .zshrc sources each from whichever
# of a few paths exists, so nothing needs editing after installing them.
brew install zsh-autosuggestions zsh-syntax-highlighting

# fzf, an interactive filter, and with it the three zsh widgets .zshrc sets up
# from it: ^R over history, ^T over file paths, M-c over directories. .zshrc
# takes the integration out of the binary with `fzf --zsh', so there is nothing
# to source by hand and nothing to keep in step with the version.
brew install fzf

brew install tmux

# various desktop stuff
brew install --cask google-chrome visual-studio-code slack iterm2 \
     telegram keepassxc

# vscode's Settings Sync, if signed in, must leave out settings and
# keybindings. chezmoi applies both now -- see vscode/readme.txt for the paths
# -- and sync writes the same files, so with both on they overwrite each other
# in whichever order they run. Extensions and the rest keep syncing. It is machine state with no
# settings.json key and no command-line form -- `code --sync off` lasts only
# the launch it is given to -- so by hand, once per machine:
#   palette > "Settings Sync: Configure..." > untick Settings and Keyboard Shortcuts

# mu/mu4e, and pass for secrets
brew install mu isync pass

# media
brew install ffmpeg yt-dlp

# Images. Nothing to install for the simple half: sips is part of macOS, at
# /usr/bin/sips, and does resize, crop, pad, rotate, flip, format conversion,
# ICC profiles and reading or setting properties. It reads and writes HEIC
# natively too, which is what iphone photos are.
#
# One trap, and it fails quietly: --out with a different extension does NOT
# convert. It writes the original bytes under the new name and only warns
# "Output file suffix should be jpg", so `file` reports JPEG for something
# called .png and the next tool dies with "Not a PNG file". Spell it out:
#
#   sips -s format png -Z 300 in.jpg --out out.png
#
# imagemagick is for what sips has no concept of -- compositing, -trim,
# -deskew, -distort Perspective, montage, -density when rasterising a PDF --
# and for the formats sips does not know. In v7 the binary is magick. convert
# is still shipped and still works, but it prints a deprecation warning on
# every run, which is noise in a script. Brew's build links libheif, libtiff,
# webp and little-cms2, so HEIC and TIFF work out of the box.
brew install imagemagick

# Scanned documents: ocrmypdf adds a searchable text layer to a scan without
# touching the image, so the page still looks like a scan and the text is
# selectable. Bottled, but it pulls in the whole pipeline -- tesseract for the
# OCR itself, ghostscript and qpdf for the PDF side, unpaper for deskewing,
# jbig2enc and pngquant for the compression -- so it is a few hundred MB.
#
# tesseract on its own carries English only. tesseract-lang is every other
# language's trained data, and the reason it is here rather than one file
# dropped into tessdata by hand: brew owns it, so it survives a tesseract
# upgrade. Russian is the one actually wanted. Usage:
#
#   ocrmypdf -l rus+eng in.pdf out.pdf
#
# Give -l only the languages the document actually has. Adding rus to a page
# that is pure Latin measurably lowers the accuracy -- tesseract starts
# returning Cyrillic look-alikes, pe for ne and a for a.
#
# Deskewing and despeckling are worth the flags: --deskew --clean gained a
# couple of points of word accuracy on a test page here. What --clean will not
# do is cut the border away, because ocrmypdf hands unpaper --layout none,
# which is exactly the switch that turns its mask and border detection off
# (see _exec/unpaper.py). To get it back, override the defaults wholesale:
#
#   ocrmypdf --clean-final --unpaper-args '--layout single --mask-scan-size 100'
#
# scantailor is the usual recommendation for the step before, and it is the
# right one for a flatbed: deskew, split a book spread, find the content box.
# It has no perspective stage, though -- the pipeline is fix_orientation,
# page_split, deskew, select_content, page_layout, output -- so it does
# nothing for the one defect a photographed page actually has. It is also a
# GUI out of a third-party tap. Skipped; see yb85/homebrew-tap if a flatbed
# ever turns up.
#
# unpaper and qpdf come in as ocrmypdf dependencies regardless; naming them
# keeps them installed-on-request, so brew autoremove leaves them behind if
# ocrmypdf ever goes. qpdf is the page surgery and earns its place on its own:
#
#   qpdf in.pdf --pages . 2-4 -- out.pdf    # also 1,3,5 / z-1 / 1-6:even
#   qpdf in.pdf --pages . 1-6,x3 -- out.pdf # x excludes, and must be comma
#                                           # joined inside the one range
#   qpdf --empty --pages a.pdf b.pdf -- merged.pdf
#   qpdf --split-pages in.pdf out.pdf       # out-1.pdf, out-2.pdf, ...
#   qpdf in.pdf --rotate=+90:1 out.pdf
#
# poppler is here for pdftotext, which answers the question that has to be
# asked before OCR -- whether the file already carries a text layer, since the
# default mode errors out when it does and the recovery is picking between
# --skip-text, --redo-ocr and --force-ocr. It brings pdfinfo, pdfimages and
# pdfseparate along. pdfgrep is the one thing none of them do: grep a whole
# directory of PDFs and report the page the hit was on.
brew install ocrmypdf tesseract-lang unpaper qpdf poppler pdfgrep

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
