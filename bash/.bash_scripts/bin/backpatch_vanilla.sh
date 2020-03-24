#!/bin/bash

set -e

# apply patch to set of branches and run make check (or whatever you want)
# usage:
# patch=~/postgres/postgresql/0001-Remove-assertion-in-reorderbuffer-that-cmax-is-stabl.patch backpatch_vanilla.sh

branches="master REL_12_STABLE REL_10_STABLE REL9_6_STABLE REL9_5_STABLE REL9_5_STABLE"
branches="REL_11_STABLE REL_10_STABLE REL9_6_STABLE REL9_5_STABLE"
currdir=$PWD

export PGSDIR=/home/ars/postgres/postgresql
export PGINAME=postgresql

for branch in $branches; do
    cd "${currdir}"
    echo "Working on ${branch}"
    git checkout ${branch}
    git pullff upstream "${branch}"
    git branch -D tmp || true
    git checkout -b tmp
    git am -3 "${patch}"
    postgres-build-full.sh -m d -t install
    cd /tmp/postgresql && make check && make -C contrib/test_decoding check
done
