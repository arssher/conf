#!/bin/bash

set -e

echo "Forcing wal switch"
for port in 15432 15433 15434; do
    :
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
# xxx obsolete
awk -f ~/.bash_scripts/bin/purge_aborts.gawk tx1 > txc1
awk -f ~/.bash_scripts/bin/purge_aborts.gawk tx2 > txc2
awk -f ~/.bash_scripts/bin/purge_aborts.gawk tx3 > txc3

echo "Collecting xacts from archive..."
docker exec node1 sh -c "ls /pg/archive/ | xargs -I{} pg_waldump /pg/archive/{} | grep -oiE 'commit_prepared \d+ MTM-\d+-\d+' | awk '{print \$3}'" | sort > xtx1_arch
docker exec node2 sh -c "ls /pg/archive/ | xargs -I{} pg_waldump /pg/archive/{} | grep -oiE 'commit_prepared \d+ MTM-\d+-\d+' | awk '{print \$3}'" | sort > xtx2_arch
docker exec node3 sh -c "ls /pg/archive/ | xargs -I{} pg_waldump /pg/archive/{} | grep -oiE 'commit_prepared \d+ MTM-\d+-\d+' | awk '{print \$3}'" | sort > xtx3_arch

echo "Collecting xacts from wal"
docker exec node1 sh -c "ls /pg/data/pg_wal/ | xargs -I{} pg_waldump /pg/data/pg_wal/{} | grep -oiE 'commit_prepared \d+ MTM-\d+-\d+' | awk '{print \$3}'" | sort > xtx1_wal
docker exec node2 sh -c "ls /pg/data/pg_wal/ | xargs -I{} pg_waldump /pg/data/pg_wal/{} | grep -oiE 'commit_prepared \d+ MTM-\d+-\d+' | awk '{print \$3}'" | sort > xtx2_wal
docker exec node3 sh -c "ls /pg/data/pg_wal/ | xargs -I{} pg_waldump /pg/data/pg_wal/{} | grep -oiE 'commit_prepared \d+ MTM-\d+-\d+' | awk '{print \$3}'" | sort > xtx3_wal

cat xtx1_arch xtx1_wal | sort | uniq > xtx1
cat xtx2_arch xtx2_wal | sort | uniq > xtx2
cat xtx3_arch xtx3_wal | sort | uniq > xtx3


# check out update histories
for node_id in 1 2 3; do
    sed --in-place -E -e "s/locally, xid ([[:digit:]]+)/locally, gid MTM-$node_id-\1/" logs${node_id}
done
