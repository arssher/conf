#!/bin/bash

# This script is used to produce git context diffs

# Supplied parameters:
# $1   $2       $3       $4       $5       $6       $7
# path old-file old-hash old-mode new-file new-hash new-mode
# 'path' is the git-tree-relative path of the file being diff'ed

old_hash="$3"
new_hash=$(git hash-object "$5")

# no change?
[ "$old_hash" = "$new_hash" ] && exit 0

# -p shows in which C function was the change
# -c for context output format
# -d produces smaller sets of changes
[ "$DIFF_OPTS" = "" ] && DIFF_OPTS='-pcd'

# echo "diff --git a/$1 b/$1"
# echo "new file mode $7"
# echo "index ${old_hash:0:7}..${new_hash:0:7}"

if ! [ -x "$(command -v colordiff)" ]; then
	diff --label a/"$1" --label b/"$1" $DIFF_OPTS "$2" "$5"
else
	diff --label a/"$1" --label b/"$1" $DIFF_OPTS "$2" "$5" | colordiff
fi
exit 0
