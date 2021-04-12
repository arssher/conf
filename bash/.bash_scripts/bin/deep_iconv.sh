#!/bin/bash

files=$(find "$(pwd)" -type f -name "*.cue")

IFS=$'\n'       # make newlines the only separator
for f in $files; do
    echo "processing $f"
    iconv -f WINDOWS-1251 -t UTF-8 "$f" | sponge "$f"
done
