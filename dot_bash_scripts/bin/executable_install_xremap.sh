#!/bin/bash
#
# Install or update the xremap system service from ~/.config/xremap, which
# chezmoi keeps current. Run as yourself, not under sudo: it builds as you and
# calls sudo per step, like restore_root.sh. Idempotent -- every step checks
# first -- so it is also the way to push an edited config.yml into /etc.
#
# Why a system service at all, and how its halves talk, is in
# ~/.config/xremap/xremap.service.

set -euo pipefail

# Pinned, so that every machine runs the same build and an upgrade is a
# commit. socket for the daemon, x11 for the bridge -- one binary does both.
version=0.15.15
features=x11,socket

src="${HOME}/.config/xremap"
user_group="xremap-${USER}"
log() { echo "install_xremap: $*"; }

for f in config.yml xremap.service; do
    if [ ! -f "${src}/${f}" ]; then
        echo "install_xremap: ${src}/${f} missing -- chezmoi apply first" >&2
        exit 1
    fi
done

# Built into a scratch root rather than ~/.cargo/bin: a copy there would sit on
# PATH ahead of /usr/local/bin and drift from the one actually running.
if [ "$(/usr/local/bin/xremap --version 2>/dev/null)" != "xremap ${version}" ]; then
    log "building xremap ${version} (${features})"
    build="${HOME}/.cache/xremap-build"
    cargo install xremap --version "${version}" --features "${features}" \
          --locked --root "${build}"
    sudo install -m 755 -o root -g root "${build}/bin/xremap" /usr/local/bin/xremap
fi

if ! getent passwd xremap >/dev/null; then
    log "creating the xremap system user"
    sudo useradd --system --no-create-home --shell /usr/sbin/nologin \
         --user-group --groups input xremap
fi

if ! getent group "${user_group}" >/dev/null; then
    log "creating ${user_group}"
    sudo groupadd --system "${user_group}"
fi
if ! id -nG "${USER}" | tr ' ' '\n' | grep -qx "${user_group}"; then
    log "adding ${USER} to ${user_group} -- takes effect at the next login"
    sudo gpasswd -a "${USER}" "${user_group}"
fi

# uinput for the input group, which only the xremap user is in. Upstream's rule
# also has TAG+="uaccess", which would hand the seat's active user -- you -- an
# ACL on uinput, and with it kernel-level key injection for every process you
# run. Left out on purpose.
rule='KERNEL=="uinput", GROUP="input", MODE="0660", OPTIONS+="static_node=uinput"'
rules=/etc/udev/rules.d/99-xremap-uinput.rules
if [ "$(cat "${rules}" 2>/dev/null)" != "${rule}" ]; then
    log "writing ${rules}"
    echo "${rule}" | sudo tee "${rules}" >/dev/null
    echo uinput | sudo tee /etc/modules-load.d/uinput.conf >/dev/null
    sudo modprobe uinput
    sudo udevadm control --reload-rules
    sudo udevadm trigger --sysname-match=uinput
fi

changed=
if ! cmp -s "${src}/config.yml" /etc/xremap/config.yml; then
    log "installing /etc/xremap/config.yml"
    sudo install -D -m 644 -o root -g root "${src}/config.yml" /etc/xremap/config.yml
    changed=1
fi
if ! cmp -s "${src}/xremap.service" /etc/systemd/system/xremap.service; then
    log "installing /etc/systemd/system/xremap.service"
    sudo install -m 644 -o root -g root "${src}/xremap.service" \
         /etc/systemd/system/xremap.service
    sudo systemctl daemon-reload
    changed=1
fi

if ! systemctl is-enabled --quiet xremap; then
    sudo systemctl enable xremap
fi
if [ -n "${changed}" ] || ! systemctl is-active --quiet xremap; then
    log "restarting xremap"
    sudo systemctl restart xremap
fi

systemctl --user daemon-reload
if ! systemctl --user is-enabled --quiet xremap-bridge; then
    systemctl --user enable xremap-bridge
fi
if id -nG | tr ' ' '\n' | grep -qx "${user_group}"; then
    systemctl --user restart xremap-bridge
else
    log "log out and back in to start the bridge; until then the remap is on everywhere, emacs included"
fi
