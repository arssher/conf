#!/bin/bash

set -e

# sudo apt-get install libav-tools cuetools shntool flac

# convert each .flac in curr dir to .mp3
for f in *.flac; do
    echo "converting ${f}"
    # you know, there used to be just ffmpeg, then war started, libav forked,
    # choosing crazy names (there are libav libraries in ffmpeg, and ffmpeg binary).
    # later they renamed to avconv, but old ffmpeg still goes on.
    avconv -i "${f}" -ab 320k -map_metadata 0 -id3v2_version 3 "${f%.*}.mp3"
    # rm -rf "${f}"
done

# split *.cue into several mp3s
# for f in *.cue; do
#     mp3splt -c "$f" "${f%.*}.mp3"
# done

# say, you have big .flac file and .cue mirror listing tracks. This splits big
# flac into separate tracks.
# for f in *.flac; do
    # cuebreakpoints "${f%.flac}.cue" | shnsplit -o flac "${f}"
# done
