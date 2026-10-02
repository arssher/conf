#!/bin/bash
#
# Pull in the private config that is deliberately not in this repo, on first
# apply only.
#
# run_once_ rather than run_onchange_ or a plain run_ script, because
# restore_private.sh is fill-in-only and there is nothing to re-check once the
# files exist. It also means a machine where the stash was missing at init time
# never retries -- which is the common case, since the stash usually arrives
# after chezmoi does. Hence the message below: on a fresh machine you run
# restore_private.sh by hand once the stash is in place.
#
# $CONFPATH comes from .bashrc, which has not been sourced when chezmoi runs
# during `chezmoi init --apply`, so this almost always no-ops on a new box. That
# is intended, not a bug to work around: guessing the stash location here would
# hardcode it into the repo.

set -euo pipefail

restore="${HOME}/.bash_scripts/bin/restore_private.sh"

if [ -z "${CONFPATH:-}" ]; then
    echo "private config: CONFPATH unset, skipping."
    echo "private config: once the stash is available, run restore_private.sh"
    exit 0
fi

if [ ! -d "${CONFPATH}/private" ]; then
    echo "private config: no stash under CONFPATH, skipping."
    echo "private config: once the stash is available, run restore_private.sh"
    exit 0
fi

if [ ! -x "${restore}" ]; then
    echo "private config: ${restore} not found, skipping" >&2
    exit 0
fi

exec "${restore}"
