#!/bin/bash
#
# Install root-owned config from the stash at $CONFPATH/etc. Needs sudo, so it is
# always run by hand -- this is the half that was split out of
# restore_private.sh, which chezmoi runs non-interactively where a password
# prompt would hang.
#
# Fill-in-only like its sibling: an existing file on the machine wins, because a
# live NetworkManager connection or fstab is more likely to be right than a
# stashed copy of unknown age.

set -euo pipefail

if [ -z "${CONFPATH:-}" ]; then
    echo "restore_root: CONFPATH is unset, nothing to restore from" >&2
    exit 1
fi

stash="${CONFPATH}/etc"

if [ ! -d "${stash}" ]; then
    echo "restore_root: no stash at ${stash}, nothing to do"
    exit 0
fi

echo "restore_root: this needs sudo"

# 0600 root:root because these hold plaintext secrets. NetworkManager does not
# enforce it -- it loads a 0644 profile without complaint, which is how this
# machine ended up with 167 world-readable ones -- but NM writes 0600 itself when
# it saves a connection, so that is the mode to restore to.
conns="${stash}/NetworkManager/system-connections"
if [ -d "${conns}" ]; then
    echo "restore_root: NetworkManager connections"
    sudo install -d -m 755 -o root -g root /etc/NetworkManager/system-connections
    for src in "${conns}"/*; do
        [ -f "${src}" ] || continue
        dest="/etc/NetworkManager/system-connections/$(basename "${src}")"
        # Tested under sudo on purpose: these directories are 0700 root:root on
        # some systems, where an unprivileged [ -e ] reports "absent" for a file
        # that exists and the guard below would silently clobber a live profile.
        if sudo test -e "${dest}"; then
            echo "  == $(basename "${src}") exists, left alone"
            continue
        fi
        sudo install -m 600 -o root -g root "${src}" "${dest}"
        echo "  -> $(basename "${src}")"
    done
fi

# openvpn configs sit next to their key material, so 0600 again.
if [ -d "${stash}/openvpn" ]; then
    echo "restore_root: openvpn"
    sudo install -d -m 755 -o root -g root /etc/openvpn
    for src in "${stash}"/openvpn/*; do
        [ -f "${src}" ] || continue
        dest="/etc/openvpn/$(basename "${src}")"
        if sudo test -e "${dest}"; then
            echo "  == $(basename "${src}") exists, left alone"
            continue
        fi
        sudo install -m 600 -o root -g root "${src}" "${dest}"
        echo "  -> $(basename "${src}")"
    done
fi

# fstab is deliberately not restored: it names this machine's filesystems by
# UUID, so a stashed copy from another box is wrong by construction. Read
# ${stash}/fstab and merge the interesting lines by hand.
if [ -f "${stash}/fstab" ]; then
    echo "restore_root: NOT restoring fstab -- UUIDs are machine-specific."
    echo "              compare by hand: diff ${stash}/fstab /etc/fstab"
fi

echo "restore_root: done"
