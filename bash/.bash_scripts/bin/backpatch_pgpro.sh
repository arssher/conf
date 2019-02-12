#!/bin/bash

# usage:
# ticket=PGPRO-2389 patch=~/postgres/postgresql/0001-Remove-assertion-in-reorderbuffer-that-cmax-is-stabl.patch backpatch_pgpro.sh

set -e
# set -o xtrace # print each line

# branches="PGPRO11_DEV PGPRO10_DEV PGPRO9_6_DEV PGPRO9_5"
branches="REL9_6_1C_DEV REL_10_1C_DEV REL_11_1C_DEV"

if [ -z "$ticket" ]; then
    echo "ticket not specified"
    exit 1
fi

if [ -z "$patch" ]; then
    echo "patch not specified"
    exit 1
fi

res_branches=""
for branch in $branches; do
    echo "Working on ${branch}"
    git checkout $branch
    git pull
    # trim _DEV branch name ending
    branch_no_dev=${branch%"_DEV"}
    branch_fix="${branch_no_dev}_${ticket}"
    git branch -D "${branch_fix}" || true
    git checkout -b "${branch_fix}"
    git am "${patch}"
    res_branches="${res_branches}${branch_fix} "
done

echo "made branches ${res_branches}"
