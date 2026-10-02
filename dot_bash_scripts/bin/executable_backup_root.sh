#!/bin/bash
#
# Push root-owned config to the stash at $CONFPATH/etc. Needs sudo; the
# counterpart of restore_root.sh.
#
# The copies land owned by you, mode 0600, so they are readable without sudo
# afterwards. The 0600 is not cosmetic given what these files hold.

set -euo pipefail

if [ -z "${CONFPATH:-}" ]; then
    echo "backup_root: CONFPATH is unset, nowhere to back up to" >&2
    exit 1
fi

stash="${CONFPATH}/etc"
echo "backup_root: saving to ${stash} (needs sudo)"

sudo -v

if [ -d /etc/NetworkManager/system-connections ]; then
    mkdir -p "${stash}/NetworkManager/system-connections"
    chmod 700 "${stash}/NetworkManager/system-connections"
    sudo find /etc/NetworkManager/system-connections -maxdepth 1 -type f \
        -exec install -m 600 -o "$(id -u)" -g "$(id -g)" {} "${stash}/NetworkManager/system-connections/" \;
    echo "  -> NetworkManager: $(find "${stash}/NetworkManager/system-connections" -type f | wc -l) profiles"
fi

if [ -d /etc/openvpn ]; then
    mkdir -p "${stash}/openvpn"
    chmod 700 "${stash}/openvpn"
    sudo find /etc/openvpn -maxdepth 1 -type f \
        -exec install -m 600 -o "$(id -u)" -g "$(id -g)" {} "${stash}/openvpn/" \;
    echo "  -> openvpn: $(find "${stash}/openvpn" -type f | wc -l) files"
fi

# Kept for reference only; restore_root.sh deliberately will not install it,
# since the UUIDs name this machine's filesystems.
sudo install -m 600 -o "$(id -u)" -g "$(id -g)" /etc/fstab "${stash}/fstab"
echo "  -> fstab (reference only)"

echo "backup_root: done"
