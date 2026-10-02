#!/bin/bash

# by Stas

docker exec node1 sh -c "ls /pg/archive/ | xargs -I{} pg_waldump /pg/archive/{} | grep -oiE 'commit_prepared \d+ MTM-\d+-\d+' | awk '{print \$3}'" > xtx1
docker exec node2 sh -c "ls /pg/archive/ | xargs -I{} pg_waldump /pg/archive/{} | grep -oiE 'commit_prepared \d+ MTM-\d+-\d+' | awk '{print \$3}'" > xtx2
docker exec node3 sh -c "ls /pg/archive/ | xargs -I{} pg_waldump /pg/archive/{} | grep -oiE 'commit_prepared \d+ MTM-\d+-\d+' | awk '{print \$3}'" > xtx3

docker exec node1 sh -c "ls /pg/data/pg_wal/ | xargs -I{} pg_waldump /pg/data/pg_wal/{} | grep -oiE 'commit_prepared \d+ MTM-\d+-\d+' | awk '{print \$3}'" >> xtx1
docker exec node2 sh -c "ls /pg/data/pg_wal/ | xargs -I{} pg_waldump /pg/data/pg_wal/{} | grep -oiE 'commit_prepared \d+ MTM-\d+-\d+' | awk '{print \$3}'" >> xtx2
docker exec node3 sh -c "ls /pg/data/pg_wal/ | xargs -I{} pg_waldump /pg/data/pg_wal/{} | grep -oiE 'commit_prepared \d+ MTM-\d+-\d+' | awk '{print \$3}'" >> xtx3

cat xtx1 | sort | uniq -c > wtx1
cat xtx2 | sort | uniq -c > wtx2
cat xtx3 | sort | uniq -c > wtx3
