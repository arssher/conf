#!/bin/bash

# Split each pdf in curr dir into single page ones

for pdf in *.pdf; do
    numpages=$(qpdf --show-npages ${pdf})
    for i in $(seq 1 ${numpages}); do
	outf="${pdf%.*}_${i}.pdf"
	echo "extracting page ${i} of file ${pdf} to ${outf}"
	pdftk "${pdf}" cat $i output "${outf}"
    done
done
