#!/bin/bash

cat all_files.txt | while read line
do
   echo "Testing $line"
   export ONLY_FILE="$line" 
   ./build_nemov5_from_repos.sh
done
