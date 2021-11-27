#!/bin/bash

set -e

# Get job artifacts from circleci. Usage:
# fetch_artifacts $job_id

job=$1

files=$(curl -s -H "Circle-Token: $CIRCLE_TOKEN" https://circleci.com/api/v1.1/project/github/zenithdb/zenith/${job}/artifacts | grep -o 'https://[^"]*')

for url in $files; do
    path="${url##*test_output/}"
    mkdir -p $(dirname ${path})
    echo "downloading ${url}"
    # -L tells to follow redirects
    curl -L -H "Circle-Token: $CIRCLE_TOKEN" -o ${path} ${url}
done
