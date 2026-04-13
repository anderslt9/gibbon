#!/bin/bash

input_files=$(ls tests/*.hs)
output_files=()

for file in $input_files; do
    stripped_file=${file##*/}
    output_files+=("test_results/${stripped_file%.hs}.txt")
done

mkdir -p test_results
# runhaskell Main.hs -i $input_files -o ${output_files[@]} $@
echo "Producing l2 AST's for input files: $input_files"
cabal v2-run gibbon-l2 -- -i $input_files -o ${output_files[@]} "$@"
