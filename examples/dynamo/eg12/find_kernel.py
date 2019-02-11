# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
# Author I. Kavcic, Met Office


''' Python helper script which returns information useful for Kernel
extraction: names of one or more Invokes which contain calls to the specified
Kernel and positions of the root Node containing the Kernel calls. '''

from __future__ import print_function
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory, Kern

# Specify API
TEST_API = "dynamo0.3"
# Specify Algorithm file name
ALG_FILE = "gw_mixed_schur_preconditioner_alg_mod.x90"
# Specify Kernel base name without the "_kernel_mod" and
# file extension
KERN_BASENAME = "matrix_vector"

# Formulate the Kernel name as it appears in the Kernel calls
KERNEL_NAME = KERN_BASENAME + "_code"
# Parse the algorithm file and return the Invoke Info objects
_, INVOKE_INFO = parse(ALG_FILE, api=TEST_API)
# Create the PSy-layer object using the Invoke info
PSY = PSyFactory(TEST_API, distributed_memory=False).create(INVOKE_INFO)

# Search through all Invokes and their Schedules for the specified Kernel
INVOKE_NAME = []
NODE_POSITION = []
for invoke in PSY.invokes.invoke_list:
    schedule = invoke.schedule
    for kernel in schedule.walk(schedule.children, Kern):
        if kernel.name.lower() == KERNEL_NAME:
            INVOKE_NAME.append(invoke.name)
            NODE_POSITION.append(kernel.root_at_depth(2).position)

print(" ")
if INVOKE_NAME:
    print("Kernel call " + KERNEL_NAME +
          " was found in ")
    for idx, name in enumerate(INVOKE_NAME):
        print("  - Invoke " + name + " at root Node position "
              + str(NODE_POSITION[idx]))
else:
    print("Kernel call " + KERNEL_NAME + " was not found in "
          + ALG_FILE)
