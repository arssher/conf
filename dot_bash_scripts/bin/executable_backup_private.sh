#!/bin/bash
#
# Push private config from $HOME to the stash at $CONFPATH/private.
#
# This is the explicit direction: unlike restore_private.sh, it does overwrite,
# because running it is a deliberate "the machine is right, save it" decision.
#
# Root-owned config is in backup_root.sh. Desktop settings are in backup_de.sh.

set -euo pipefail

if [ -z "${CONFPATH:-}" ]; then
    echo "backup_private: CONFPATH is unset, nowhere to back up to" >&2
    exit 1
fi

stash="${CONFPATH}/private"
echo "backup_private: saving to ${stash}"

# save <source in $HOME> <stash-relative destination>
# Everything here is sensitive, so 0600 in the stash too -- a stash on a
# filesystem with a loose umask would otherwise end up world readable.
save() {
    local src="${HOME}/$1" dest="${stash}/$2"

    if [ ! -f "${src}" ]; then
        echo "  -- ~/$1 absent, skipped"
        return 0
    fi

    install -D -m 600 "${src}" "${dest}"
    echo "  -> $2"
}

save .global_vars            .global_vars
save .bash_scripts/aliases.sh .bash_scripts/aliases.sh
save .ssh/config             .ssh/config
save .persistent_history     .persistent_history
save .psql_history           .psql_history

echo "backup_private: done"
