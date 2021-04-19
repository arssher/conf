#!/bin/bash

set -e

echo "Copying desktop environment files to the CONFPATH (${CONFPATH})..."

dconf dump / > "${CONFPATH}/de.dconf"
