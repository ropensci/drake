#!/bin/bash

for file in *.out; do
    echo $file
    tail -n 20 $file
    printf "\n\n"
done
