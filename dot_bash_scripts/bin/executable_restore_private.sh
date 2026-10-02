#!/bin/bash
#
# Install private config from the stash at $CONFPATH/private into $HOME.
#
# Only $HOME is touched. Root-owned config lives in restore_root.sh, because
# chezmoi also runs this script non-interactively during `apply`, and a sudo
# password prompt there would hang with nothing to type into.
#
# Two rules this script follows, both learned the hard way:
#
#  1. Fill-in-only. An existing file is never overwritten. These files get edited
#     in place -- .global_vars especially -- and the old `cp -rp` would silently
#     replace a newer home copy with a stale stash copy. To push home -> stash,
#     run backup_private.sh; that direction is always explicit.
#
#  2. Modes are set here, not inherited. The stash copies are whatever mode the
#     filesystem that carries them happened to give them, and cp -p would
#     propagate that. Everything here is sensitive, so it lands 0600 under a
#     0700 directory regardless of the source.

set -euo pipefail

if [ -z "${CONFPATH:-}" ]; then
    echo "restore_private: CONFPATH is unset, nothing to restore from" >&2
    exit 0
fi

stash="${CONFPATH}/private"

if [ ! -d "${stash}" ]; then
    echo "restore_private: no stash at ${stash}, skipping"
    exit 0
fi

echo "restore_private: installing from ${stash}"

# ~/.ssh must be 0700 whether or not we create it: a traversable directory exposes
# whatever is inside to every local account.
mkdir -p "${HOME}/.ssh"
chmod 700 "${HOME}/.ssh"

# fill <stash-relative source> <destination> <mode>
fill() {
    local src="${stash}/$1" dest="$2" mode="$3"

    if [ ! -f "${src}" ]; then
        echo "  -- $1 not in stash, skipped"
        return 0
    fi
    if [ -e "${dest}" ]; then
        echo "  == ${dest} exists, left alone"
        return 0
    fi

    install -D -m "${mode}" "${src}" "${dest}"
    echo "  -> ${dest} (${mode})"
}

# Shell environment.
fill .global_vars "${HOME}/.global_vars" 600

# Sourced by .bashrc behind a [ -f ] guard.
fill .bash_scripts/aliases.sh "${HOME}/.bash_scripts/aliases.sh" 600

# ssh client config.
fill .ssh/config "${HOME}/.ssh/config" 600

# Shell and psql history. Restored on a new machine, never touched again --
# the shell appends to .persistent_history on every prompt, so overwriting it
# later would throw away everything since the last backup.
fill .persistent_history "${HOME}/.persistent_history" 600
fill .psql_history "${HOME}/.psql_history" 600

echo "restore_private: done"
echo "restore_private: desktop settings are separate, see restore_de.sh"
echo "restore_private: root-owned config is separate, see restore_root.sh"
