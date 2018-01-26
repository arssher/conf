#!/bin/bash

# Make pdfs in current directory readable on kindle

set -e

shopt -s nullglob # don't enter if no pdf
for pdf in *.pdf; do
    if [[ ${pdf} != *"decrypted"* ]]; then
	echo "Decrypting ${pdf} to ${pdf%.*}_decrypted.pdf"
	qpdf --decrypt "${pdf}" "${pdf%.*}_decrypted.pdf"
    fi
    # qpdf 
done
