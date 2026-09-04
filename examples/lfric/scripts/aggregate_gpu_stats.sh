#!/usr/bin/env bash

# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Check a filename argument is given
if [[ $# -ne 1 ]]; then
    echo "Usage: gpu_stats.sh <filename>"
    exit 1
fi
filename=$1
if [[ ! -r "$filename" ]]; then
    echo "$filename does not exist or is not readable" 
    exit 1
fi

 
count_uniq() {
    echo -n "$1: "
    grep "$1" $filename | sort | uniq | wc -l
}

check_above() {
    value=$(grep "$1" $filename | sort | uniq | wc -l)
    if [[ $value -lt $2 ]]; then
        echo
        echo "Error: Number of $1 is below $2"
        exit 1
    elif [[ $value -gt $2 ]]; then
        echo
        echo "Improvement: Number of $1 has increased to $value"
    fi
}

echo " --- First we need to be able to modify kernels ---"
count_uniq "Module-inline successful"
count_uniq "Module-inline failed"
echo -n "   -> "
count_uniq "because it accesses data from its outer scope"
echo
echo " --- Then we need to inline them, or fallback to GPU routine annotations ---"
count_uniq "Inline successful"
count_uniq "Inline failed"
count_uniq "Annotation successful"
count_uniq "Annotation failed"
echo -n "   -> "
count_uniq "accesses the imported symbol"
echo -n "   -> "
count_uniq "calls another routine"
echo -n "   -> "
count_uniq "calls intrinsic"
echo -n "   -> "
count_uniq "only supports the transformation of a MATMUL operation when"

echo
echo " --- Then offload each loop with kernels inside ---"
count_uniq "Offload independent loop"
count_uniq "Offload with dof loop"
count_uniq "Offload with atomics"
count_uniq "Offload with cell colouring"
count_uniq "Offload with cell tile-colouring"
count_uniq "Failed to offload"
count_uniq "Added inner loop nested parallelism"
count_uniq "Added OMP threading"

check_above "Module-inline successful" 283
check_above "Offload independent loop" 83
check_above "Offload with cell colouring" 40
