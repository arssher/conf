#!/bin/bash

set -e

for port in 15432 15433 15434; do
    psql -d "dbname=regression user=postgres host=127.0.0.1 port=${port} application_name=mtm_admin" -c "select pg_switch_wal()"
done

echo "Collecing mtm logs..."
docker logs node1 > logs1 2>&1
docker logs node2 > logs2 2>&1
docker logs node3 > logs3 2>&1

cat logs1 | grep -oE 'TXFINISH: MTM.+? [c|a]' | sort | uniq -c > tx1
cat logs2 | grep -oE 'TXFINISH: MTM.+? [c|a]' | sort | uniq -c > tx2
cat logs3 | grep -oE 'TXFINISH: MTM.+? [c|a]' | sort | uniq -c > tx3

# to leave only committed xacts,
awk -f ~/.bash_scripts/bin/purge_aborts.gawk tx1 > txc1
awk -f ~/.bash_scripts/bin/purge_aborts.gawk tx2 > txc2
awk -f ~/.bash_scripts/bin/purge_aborts.gawk tx3 > txc3

echo "Collecing mtm logs from wal..."
docker exec node1 sh -c "ls /pg/archive/ | xargs -I{} pg_waldump /pg/archive/{} | grep -oiE 'commit_prepared \d+ MTM-\d+-\d+' | awk '{print \$3}'" | sort > xtx1
docker exec node2 sh -c "ls /pg/archive/ | xargs -I{} pg_waldump /pg/archive/{} | grep -oiE 'commit_prepared \d+ MTM-\d+-\d+' | awk '{print \$3}'" | sort > xtx2
docker exec node3 sh -c "ls /pg/archive/ | xargs -I{} pg_waldump /pg/archive/{} | grep -oiE 'commit_prepared \d+ MTM-\d+-\d+' | awk '{print \$3}'" | sort > xtx3
