#!/usr/bin/env bash
set -euo pipefail

while true; do
    inotifywait -e move,modify,create,delete -r ./{org,static} && \
    sassc ./static/styles/base.scss ./static/styles/base.css && \
    make clean && make
done
