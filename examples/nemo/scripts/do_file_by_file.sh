#!/bin/bash

cat all_files.txt | while read line
do
   echo "Testing $line"
   export ONLY_FILE="$line" 
   # rm nemov5/cfgs/ORCA2_ICE_PISCES_psycloned/BLD/obj/${line/.f90/.o}
   # ./build_nemov5_from_repos.sh
   ./testscript.sh
   pwd
   # rm nemov5/cfgs/ORCA2_ICE_PISCES_psycloned/BLD/obj/${line/.f90/.o}
   echo "Finish $line"
done
