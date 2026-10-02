#!/bin/bash
#
# Dispatch to the real script of the same name in the private stash.
#
# The restore_*/backup_* scripts shuttle content between $CONFPATH and this
# machine. They describe the stash's layout, so they live there rather than in
# this repo. The names stay on PATH as symlinks to this wrapper, so
# `restore_private.sh` and friends keep working as commands, and anything that
# calls them by name -- bootstrap.sh, the chezmoi hook -- needs no change.
#
# One wrapper plus a symlink per name, rather than six near-identical wrappers:
# the dispatch is driven by $0, so there is a single place to fix.

set -euo pipefail

self="$(basename "$0")"

if [ "${self}" = "private-dispatch.sh" ]; then
    echo "private-dispatch.sh is not meant to be run directly." >&2
    echo "It is the target of symlinks named after the scripts in" >&2
    echo "\${CONFPATH}/bin, and dispatches to them by \$0." >&2
    exit 64
fi

if [ -z "${CONFPATH:-}" ]; then
    echo "${self}: CONFPATH is unset, so the private stash cannot be located." >&2
    echo "${self}: it is normally set in .bashrc." >&2
    exit 1
fi

real="${CONFPATH}/bin/${self}"

if [ ! -x "${real}" ]; then
    echo "${self}: not found at ${real}" >&2
    echo "${self}: the private stash is not available on this machine yet." >&2
    exit 1
fi

exec "${real}" "$@"
