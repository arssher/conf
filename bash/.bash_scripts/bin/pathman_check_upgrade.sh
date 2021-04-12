#!/bin/bash

set -e

port=5442

cd ~/postgres/pg_pathman/
git checkout d34a77e
USE_PGXS=1 make install
pg.sh -p $port 0
USE_PGXS=1 PGPORT=$port make installcheck

git checkout 6c9d435
USE_PGXS=1 make install
pg_ctl -D /tmp/data0 restart
psql -c -p $PORT -c "ALTER EXTENSION pg_pathman UPDATE;"
