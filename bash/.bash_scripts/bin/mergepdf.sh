#!/bin/bash

set -e

i=600
convert ars_passport.pdf sem_polosh.pdf \
	-compress jpeg -quality 100 \
	-density ${i}x${i} -units PixelsPerInch \
	-resize $((i*827/100))x$((i*1169/100)) \
	-repage $((i*827/100))x$((i*1169/100)) \
	ars_passport_full.pdf
