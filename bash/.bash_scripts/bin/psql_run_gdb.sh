#!/bin/bash

# Adapted from http://akorotkov.github.io/blog/2015/08/26/psql-gdb-attach/

IFS=''

while read line
do
	# Extended display off
	if [[ $line =~ ^\ +([0-9]+) ]]; then
		PID=${BASH_REMATCH[1]}
		break
	fi
	# Extended display on
	if [[ $line =~ ^pg_backend_pid.*\ ([0-9]+) ]]; then
		PID=${BASH_REMATCH[1]}
		break
	fi
done

emacs --eval "(gdb \"gdb -i=mi -p ${PID}\")" &
