#!/usr/bin/env bash

# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2026, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Authors: S. Siso, STFC Daresbury Lab

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

check_above "Module-inline successful" 277
check_above "Offload independent loop" 88
check_above "Offload with cell colouring" 40
