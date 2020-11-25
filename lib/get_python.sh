#!/usr/bin/bash

# This scripts find an executable python command. On
# certain platforms that have only python3 installed,
# 'python' is not found.

# This script first tests the various python commands return
# by which (if any), then standard locations on linux. Note
# we redirect to /dev/null in case that 'which' does not exist
for p in $(which python python3 python2 2>/dev/null) \
         /usr/bin/python  \
         /usr/bin/python3 \
         /usr/bin/python2; do

    # Necessary in case that 'python' is a directory
    if [[ -f "$p" && -x $(realpath "$p") ]]; then
        echo $p;
        exit
    fi
done

echo "Cannot find working python"

exit -1