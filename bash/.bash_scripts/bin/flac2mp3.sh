# for f in *.flac; do
    # avconv -i "$f" -ab 320k -map_metadata 0 -id3v2_version 3 "${f%.*}.mp3"
    # rm -rf "$f"
# done

# for f in *.cue; do
#     mp3splt -c "$f" "${f%.*}.mp3"
# done

for f in *.flac; do
    cuebreakpoints "${f}.cue" | shnsplit -o flac "$f"
done
