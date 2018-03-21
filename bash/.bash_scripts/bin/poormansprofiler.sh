#!/bin/bash

# adapted from https://poormansprofiler.org/ for flamegraph.pl
# example:
# poormansprofiler.sh > poor.stacks
# flamegraph.pl --color=yellow --title="Poor man's flamegraph" < poor.stacks > poor.svg

nsamples=20
sleeptime=1
pid=`ps aux | grep '[p]ostgres: ars' | tail -n 1 | awk '{print $2}'`

for x in $(seq 1 $nsamples)
  do
    gdb -ex "set pagination 0" -ex "thread apply all bt" -batch -p $pid
    sleep $sleeptime
  done | \
awk '
  BEGIN { s = ""; fname = "";}
  /^Thread/ { if (s != "" ) {print s;} s = ""; } # found new stacktrace, print old and reset line
  /^\#/ {
    if (index($0, " in ") != 0) {
      fname = $4 # like #1  0x000000000085b807 in WaitEventSetWaitBlock...
    }
    else {
      fname = $2 # like  #0  epoll_pwait (fd=3,...
    }
    if (s != "" ) { s = fname";"s} else { s = fname }
  }
  END { print s }'  | \
    sort | uniq -c | sort -r -n -k 1,1 | sed -E 's/([0-9]+) (.*)/\2 \1/'
# sort reversed, numerically, by first field;
# sed moves count by `uniq` (first group) to the right of the rest of line (second group)
