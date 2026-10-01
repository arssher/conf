#!/bin/bash

# Basic pathman backpatch
# usage:
# TICKET=PGPRO-2389 backpatch_pathman.sh

set -e
# set -o xtrace # print each line

branches="PGPRO11_DEV PGPRO10_DEV PGPRO9_6_DEV PGPRO9_5"

if [ -z "$TICKET" ]; then
    echo "TICKET not specified"
    exit 1
fi

done_branches=""
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
    # git push origin "${branch_fix}"
    done_branches="${done_branches}${branch_fix} "
done

echo "done branches ${done_branches}"
