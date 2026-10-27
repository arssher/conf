#!/bin/bash

# Builds emacs from source and installs it into /usr/local. Debian/Ubuntu;
# source repos must be enabled (deb-src in /etc/apt/sources.list.d).
#
# From source because debian is a long way behind: trixie ships 30.1 and even
# sid only has 30.2, while markdown-ts-mode -- the reason for wanting 31 --
# arrived in 31.1.
#
# Into /usr/local, where this used to install into ~/opt. ~/opt/bin is
# APPENDED to PATH by ~/.global_vars, so /usr/bin/emacs from the distro
# package won the lookup anyway, and since .global_vars is sourced only by the
# interactive halves of .bashrc and .zshrc, the desktop session never saw
# ~/opt/bin at all -- a GUI launcher would have kept starting the old one.
# /usr/local/bin precedes /usr/bin everywhere, that session included.
#
# dpkg does not own /usr/local, so the distro package stays installed and
# untouched: /usr/bin/emacs still runs it if this build turns out badly.

set -e

ver=31.1
src="${HOME}/opt/src"

# Hold the sudo timestamp open for the whole run, so there is one password
# prompt at the start and none later. sudo's timeout is 15 minutes by default
# and the compile below takes rather longer than that, so without this the
# final `sudo make install` sits on a prompt -- and if you walked away when
# the build started, it has been sitting there ever since.
#
# A refresher rather than a bigger timestamp_timeout: that is settable only in
# sudoers, deliberately, since a command-line override would defeat it. -n on
# the refresh so that a timestamp which died anyway fails quietly instead of
# prompting from the background, where the prompt would be invisible. The
# kill -0 is in case this script is killed outright and the trap never runs.
sudo -v
while kill -0 "$$" 2>/dev/null; do sudo -n -v 2>/dev/null; sleep 60; done &
sudo_keepalive=$!
trap 'kill "${sudo_keepalive}" 2>/dev/null' EXIT

sudo apt-get update
# The deps for whatever emacs debian packages, which covers this one too:
# libgccjit for native compilation and libtree-sitter-dev both arrive with it.
sudo apt-get -y build-dep emacs

mkdir -p "${src}"
cd "${src}"
# -nc, and the tarball is kept rather than deleted, so a re-run after a failed
# build does not download it again.
wget -nc "https://ftp.gnu.org/pub/gnu/emacs/emacs-${ver}.tar.xz"
tar -xf "emacs-${ver}.tar.xz"
cd "emacs-${ver}"

# gtk3 rather than pgtk: the session here is X11.
# native-compilation=aot compiles the bundled lisp now instead of lazily on
# first use. It is most of the build time, and the reason for the -j.
./configure \
    --with-native-compilation=aot \
    --with-tree-sitter \
    --with-x-toolkit=gtk3
make -j"$(nproc)"
sudo make install

# Tree-sitter grammars. Debian packages none of them and emacs bundles none --
# it knows only the recipes, which each mode's library registers when loaded,
# pinned to a commit, and the compiling happens here, with cc and git. They
# land in ~/.emacs.d/tree-sitter/, which is not version-specific, so this
# survives the next emacs. A language added later needs no re-run: turning
# its ts mode on by hand offers to build the grammar there and then
# (treesit-auto-install-grammar).
#
# Each entry is the library that registers the recipe, then its languages.
# Markdown needs two grammars, the block one and the inline one, from two
# directories of a single repo. Not go, rust or c: those stay on go-mode,
# rustic and lsp's c-mode, see lang.el.
#
# Explicit path: the emacs just installed, not whatever PATH resolved to
# before. -Q because this must not depend on the init files loading cleanly
# under a brand new emacs -- and lang.el checks for these grammars before it
# turns their ts modes on, so the init is the thing waiting on them.
/usr/local/bin/emacs -Q -batch --eval '(progn
  (dolist (spec (quote ((markdown-ts-mode markdown markdown-inline)
                        (sh-script bash)
                        (json-ts-mode json)
                        (yaml-ts-mode yaml)
                        (toml-ts-mode toml)
                        (python python))))
    (require (car spec))
    (dolist (lang (cdr spec))
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang)))))'

# One-time start, so packages.el installs the elpa packages it asks for.
#
# -l on the init file, because --batch implies -q: a plain `emacs -batch`
# loads no init at all and so installed nothing. That is what this line did
# for years -- the packages were really being installed by the first
# interactive start instead.
/usr/local/bin/emacs -batch -l "${HOME}/.emacs.d/init.el"
