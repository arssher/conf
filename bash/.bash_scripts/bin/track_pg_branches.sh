#!/bin/bash

set -e

for branch in master REL_11_STABLE REL_10_STABLE REL9_6_STABLE REL9_5_STABLE REL9_4_STABLE; do
    git branch -D $branch
    git branch --track $branch upstream/$branch
done
