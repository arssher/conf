#!/bin/bash

# usage:
# TICKET=PGPRO-2389 backpatch_pathman.sh

set -e
# set -o xtrace # print each line

branches="PGPRO11_DEV PGPRO10_DEV PGPRO9_6_DEV PGPRO9_5"

if [ -z "$TICKET" ]; then
    echo "TICKET not specified"
    exit 1
fi

pushed_branches=""
for branch in $branches; do
    echo "Working on ${branch}"
    git checkout $branch
    git pull
    # trim _DEV branch name ending
    branch_no_dev=${branch%"_DEV"}
    branch_fix="${branch_no_dev}_${TICKET}"
    git checkout -b "${branch_fix}"
    git fetch pg_pathman
    git subtree pull --prefix contrib/pg_pathman pg_pathman master
    git push origin "${branch_fix}"
    pushed_branches="${pushed_branches}${branch_fix} "
done

echo "pushed branches ${pushed_branches}"
