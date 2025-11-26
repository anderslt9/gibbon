#!/bin/bash

input_files=$(ls tests/*.hs)
output_files=()
for file in $input_files; do
    stripped_file=${file##*/}
    output_files+=("test_results/${stripped_file%.hs}.txt")
done

mkdir -p test_results
runhaskell grammar.hs -i $input_files -o ${output_files[@]}
