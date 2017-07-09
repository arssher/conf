#!/bin/bash

dirs=`find /d/perf_jit_res3 -type d -name "*"`

for d in $dirs; do
    cd $d
    echo $d
    for i in `seq 1 5`; do
	cat out-$i.perf-script | ~/Downloads/FlameGraph/stackcollapse-perf.pl --kernel > out-$i.perf-folded
	qname=`ls *.sql | tr -d '\n'`
	echo $qname
	# grep -E -v "llvm::|llvmmix::" out-$i.perf-folded | ~/Downloads/FlameGraph/flamegraph.pl > "${qname}-${i}_filtered".svg
    done
done
