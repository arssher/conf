#!/bin/bash

set -e

# convert each .mov in curr dir to .mp4
for f in *.MOV; do
    echo "converting ${f}"
    # try also -vcodec h264 -acodec mp2
    # crf ranges 0-53, 0 is best, 51 worst, 23 is default.
    ffmpeg -i "${f}" -crf 33 "${f%.*}.mp4"
    # rm -rf "${f}"
done
