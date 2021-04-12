#!/bin/bash

# adapted from https://poormansprofiler.org/ for flamegraph.pl

show_help() {
    cat <<EOF
    Usage:
      poormansprofiler.sh [-n <nsamples>] [-s <sleeptime>] <-p pid> > poor.stacks
      flamegraph.pl --color=yellow --title="Poor man's flamegraph" < poor.stacks > poor.svg
      It is recommended to run gdb-add-index.sh to decrease gdb start up time.
EOF
    exit 0
}

nsamples=100
sleeptime=0.1
pid=`ps aux | grep '[p]ostgres: ars' | tail -n 1 | awk '{print $2}'`

OPTIND=1 # reset opt counter, it is always must be set to 1
# each symbol is option name; if there is colon after, it has value
# the first colon would mean silent mode (e.g. suppress illegal option -- p)
while getopts ":p:n:s:" opt; do # the result will be stored in $opt
    case $opt in
	h) # bracket is a part of case syntax, you know
	    show_help
	    exit 0
	    ;;
	p)
	    pid=$OPTARG
	    ;;
	n)
	    nsamples=$OPTARG
	    ;;
	s)
	    sleeptime=$OPTARG
	    ;;
	\?) # match '?'
	    show_help >&2
	    exit 1
	    ;;
    esac
done

for x in $(seq 1 $nsamples)
do
    pid=$(ps aux | grep '[p]ostgres:' | shuf -n 1 | awk '{print $2}')
    gdb -nx -ex "set pagination 0" -ex "thread apply all bt" -batch -p $pid
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
