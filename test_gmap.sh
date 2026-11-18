#!/usr/bin/env bash

set -u

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
maps_dir="$repo_root/gibbon-compiler/examples/maps"

cd "$repo_root/gibbon-compiler"
export GIBBONDIR="$repo_root"

if command -v gibbon >/dev/null 2>&1; then
    run_gibbon() {
        gibbon -- "$@"
    }
else
    run_gibbon() {
        cabal run exe:gibbon -- "$@"
    }
fi

for source_file in "$maps_dir"/*.hs; do
    [ -e "$source_file" ] || continue

    file_name=$(basename "$source_file")
    base_name=${file_name%.hs}

    echo "Running $file_name"
    run_gibbon -v3 -r --packed "examples/maps/$file_name" \
        &> "$repo_root/${base_name}_results.txt"
done
