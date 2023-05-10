#!/bin/bash

# helper to add aliases for fundamental vscode keys/commands (for keys having a
# lot commands, like escape).

> vscode_remap.json

function remap {
    from=$1
    to=$2
    echo mapping $from to $to
    echo "// default '${from}' remapped to '${to}'" >> vscode_remap.json
    cat default_vscode_kb.json  | jq ". | map(select(.key == \"${from}\"))" | sed "s/\"key\": \"${from}\"/\"key\": \"${to}\""/ | tee -a vscode_remap.json
}

rm -f vscode_remap.json
# remap up alt+i
# remap left alt+j
# remap down alt+k
remap right alt+l
