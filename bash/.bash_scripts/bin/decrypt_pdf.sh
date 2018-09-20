#!/bin/bash

# Make pdfs in current directory readable on kindle

set -e

decrypt_pdf_func() {
    path=$1
    echo "Working on directory ${path}..."
    shopt -s nullglob # don't enter loop if no pdf
    cd "${1}"
    for pdf in *.pdf; do
	if [[ ${pdf} != *"decrypted"* ]]; then
	    echo "Decrypting ${pdf} to ${pdf%.*}_decrypted.pdf"
	    qpdf --decrypt "${pdf}" "${pdf%.*}_decrypted.pdf"
	fi
    done
}

export -f decrypt_pdf_func
find `pwd` -type d | xargs -n 1 -I {} bash -c 'decrypt_pdf_func "$@"' _ {}
