#!/bin/bash

set -e

# This script generates gdb indexes for specified pid and puts them to object
# files

show_help() {
    cat <<EOF
    Usage: gdb-add-index.sh -p <pid>
EOF
    exit 0
}

OPTIND=1 # reset opt counter, it is always must be set to 1
# each symbol is option name; if there is colon after, it has value
# the first colon would mean silent mode (e.g. suppress illegal option -- p)
while getopts ":p:" opt; do # the result will be stored in $opt
    case $opt in
	h) # bracket is a part of case syntax, you know
	    show_help
	    exit 0
	    ;;
	p)
	    pid=$OPTARG
	    ;;
	\?) # match '?'
	    show_help >&2
	    exit 1
	    ;;
    esac
done

# generate indexes
rm -rf gdb; mkdir gdb
gdb -batch -p $pid -ex "save gdb-index gdb/"

# and put them to the object files
objs=$(gdb -p $pid < /dev/null | grep 'Reading symbols from' | sed -E 's/.*Reading symbols from(.*)...done./\1/')

for obj in $objs; do
    obj_fname=$(basename $obj)
    if [ ! -f "gdb/$obj_fname.gdb-index" ]; then
	echo "Index for file $obj_fname not found!"
	continue
    fi
    echo "Merging gdb/$obj_fname.gdb-index into ${obj}..."
    obj_path=$(dirname $obj)
    # backup the file before augumenting
    sudo cp $obj "$obj_path/${obj_fname}.backup"
    sudo objcopy --add-section ".gdb_index=gdb/$obj_fname.gdb-index" \
	 --set-section-flags .gdb_index=readonly $obj $obj
done
