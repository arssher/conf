#!/bin/bash

dirs=`find /d/perf_jit_res2 -type d -name "2017-06-09_09-24-43*"`

for d in $dirs; do
    cd $d
    echo $d
    for i in `seq 1 5`; do
	cat out-$i.perf-script | ~/Downloads/FlameGraph/stackcollapse-perf.pl --kernel > out-$i.perf-folded
	cat out-$i.perf-folded | ~/Downloads/FlameGraph/flamegraph.pl > q01-$i.svg
	grep -E -v "llvm::|llvmmix::" out-$i.perf-folded | ~/Downloads/FlameGraph/flamegraph.pl > "q01-${i}_filtered".svg
    done
done
