#!/bin/bash

# test=007_add_stop_node
# test=003_basic_recovery
# test=004_recovery
test=001_regress
# test=001_truncate
# test=006_pgbenchdl
# test=012_bugfixes
# test=009_identity_func
# logf=bf_buster_basic_recovery.log
logf=bf.log
for i in `seq 1 4`; do
    echo "extracting ${test}_node${i}.log"
    sed -n "/=====.*${test}_node${i}.log/,/==========/p" ${logf} >"${test}_node${i}.log"
done
echo "extracting ${test}"
sed -n "/====.*${test} ============/,/================= pgsql/p" ${logf} >"${test}"
