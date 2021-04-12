#!/bin/bash

set -e

# usage: norsu_pgxs_installcheck.sh master REL_10_STABLE REL9_5_STABLE

pgxs() {
  echo "PG: $1"
  # 'norsu pgxs' runs USE_PGXS=1 PG_CONFIG=path/to/pg_config make;
  # -R runs temp instance

  norsu pgxs $1 -- clean install -j4 && norsu pgxs $1 -R -- installcheck
  norsu pgxs $1 -- clean install -j4 && norsu pgxs $1 -R -- python_tests
  # one test
  # norsu pgxs $1 -- clean install -j4 && CASE=test_replication norsu pgxs $1 -R -- python_tests
}

for revision in "$@"
do
    pgxs "${revision}"
done
