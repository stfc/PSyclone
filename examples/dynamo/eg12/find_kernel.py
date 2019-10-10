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


'''
Python helper script which returns the information useful for Kernel
extraction: names of one or more Invokes which contain calls to the specified
Kernel and schedules of those Invokes.

Use: '$ python <path/to/script/>find_kernel.py'

The user-specified settings can be modified in the first section:
TEST_API - PSyclone API (the example here "dynamo0.3"),
ALG_PATH - Relative path to the Algorithm file from the location
           where this script is run,
ALG_NAME - Algorithm file name to be searched for Kernel calls,
KERNEL_BASENAME - Base name of the Kernel to be searched (without the
                  "_kernel_mod" and file extension),
TRANSFORM - Switch for applying transformations to PSy layer before
           searching for the Kernel call,
TRANS_SCRIPT - Name of the transformation script which applies PSyclone
               transformations to the code. A valid script file must
               contain a 'trans' function which modifies the PSy object.
'''

from __future__ import print_function
import os
import importlib
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory, Kern

# ============= 1. User-defined settings ==================================== #
#
# Specify API
TEST_API = "dynamo0.3"
# Specify path to the Algorithm file from this script's location
ALG_PATH = "../code"
# Specify Algorithm file name
ALG_NAME = "gw_mixed_schur_preconditioner_alg_mod.x90"
# Specify the Kernel base name without the "_mod" and
# file extension
KERNEL_BASENAME = "matrix_vector_kernel"
# Specify whether to apply transformations before looking for the Kernel call
# position
TRANSFORM = False
# Specify name of the transformation script
TRANS_SCRIPT = "colouring_and_omp"

# ============= 2. Manage names, paths and transformation script imports ==== #
#
# Formulate the Kernel name as it appears in the Kernel calls
KERNEL_NAME = KERNEL_BASENAME + "_code"
# Join path to Algorithm file and its name
ALG_FILE = os.path.join(os.path.abspath(ALG_PATH), ALG_NAME)

# If transformation option is enabled, try to import the specified
# transformation script as a Python module
TRANSMOD = None
if TRANSFORM:
    try:
        TRANSMOD = importlib.import_module(TRANS_SCRIPT)
    except ImportError:
        print("\nOptimisation error: did not find the transformation script '"
              + TRANS_SCRIPT + "'. No transformations will be applied.")

# ============= 3. Search for the Kernel call =============================== #
#
# Parse the algorithm file and return the Invoke info objects
_, INVOKE_INFO = parse(ALG_FILE, api=TEST_API)
# Create the PSy object which contains all Invoke calls
PSY = PSyFactory(TEST_API, distributed_memory=False).create(INVOKE_INFO)

# Apply transformations to the PSy object if this option is enabled and
# the transformation script was loaded successfully
if TRANSMOD:
    PSY = TRANSMOD.trans(PSY)

# Search through all Invokes and their Schedules for the specified Kernel
# call. Create lists of Invoke names which contain the specified call.
INVOKE_NAME = []
for invoke in PSY.invokes.invoke_list:
    schedule = invoke.schedule
    for kernel in schedule.walk(Kern):
        if kernel.name.lower() == KERNEL_NAME and \
          invoke.name not in INVOKE_NAME:
            INVOKE_NAME.append(invoke.name)

# Print names and schedules of Invokes which contain the call to the
# specified Kernel
if INVOKE_NAME:
    print("\nKernel call '" + KERNEL_NAME +
          "' was found in ")
    for idx, name in enumerate(INVOKE_NAME):
        print("\n- Invoke '" + name + "' with the Schedule: ")
        schedule = PSY.invokes.get(name).schedule
        schedule.view()
else:
    print("Kernel call '" + KERNEL_NAME + "' was not found in "
          + ALG_NAME)
