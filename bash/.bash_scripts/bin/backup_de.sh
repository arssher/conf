#!/bin/bash

# This seems to work with at least mint and gnome.

set -e

echo "Copying desktop environment files to the CONFPATH (${CONFPATH})..."

dconf dump / > "${CONFPATH}/de.dconf"
