#!/bin/bash

set -e

# reduce size of each jpg in current dir
for f in *.JPG; do
    echo "converting ${f}"
    mogrify -quality 80 "${f}"
done
