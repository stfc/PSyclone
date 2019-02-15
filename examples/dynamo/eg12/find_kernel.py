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
Kernel and positions of the root Nodes containing the Kernel calls.

Use: '$ python <path/to/script/>find_kernel.py'

The user-specified settings can be modified in the first section:
TEST_API - PSyclone API (the example here "dynamo0.3"),
ALG_NAME - Algorithm file name to be searched for Kernel calls,
ALG_PATH - Path to the Algorithm file (absolute or relative from the
           location where this script is run),
KERNEL_BASENAME - Base name of the Kernel to be searched (without the
                  "_kernel_mod" and file extension),
OPTIMISE - Switch for applying optimisations to PSy layer before
           searching for the Kernel call,
OPT_SCRIPT - Name of the optimisation script which applies PSyclone
             transformations to the code. A valid script file must
             contain a 'trans' function which modifies the PSy object.
'''

from __future__ import print_function
import os
import importlib
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory, Kern

# =============== 1. User-defined settings ================================== #
#
# Specify API
TEST_API = "dynamo0.3"
# Specify Algorithm file name
ALG_NAME = "gw_mixed_schur_preconditioner_alg_mod.x90"
# Specify path to the Algorithm file from this script's location
ALG_PATH = "."
# Specify the Kernel base name without the "_kernel_mod" and
# file extension
KERNEL_BASENAME = "dg_matrix_vector"
# Specify whether to apply optimisations before looking for the Kernel call
# position
OPTIMISE = False
# Specify name of the optimisation script
OPT_SCRIPT = "colouring_and_omp"

# =============== 2. Manage names, paths and optimisation script imports ==== #
#
# Formulate the Kernel name as it appears in the Kernel calls
KERNEL_NAME = KERNEL_BASENAME + "_code"
# Join path to Algorithm file and its name
ALG_FILE = os.path.join(ALG_PATH, ALG_NAME)

# If optimisation option is enabled, try to import the specified optimisation
# transformation script as a Python module
OPTMOD = None
if OPTIMISE:
    try:
        OPTMOD = importlib.import_module(OPT_SCRIPT)
    except ImportError:
        print("\nOptimisation error: did not find the optimisation script '"
              + OPT_SCRIPT + "'. No optimisations will be applied.")

# =============== 3. Search for the Kernel call ============================= #
#
# Parse the algorithm file and return the Invoke info objects
_, INVOKE_INFO = parse(ALG_FILE, api=TEST_API)
# Create the PSy object which contains all Invoke calls
PSY = PSyFactory(TEST_API, distributed_memory=False).create(INVOKE_INFO)

# Apply optimisations to the PSy object if this option is enabled and
# the optimisation script was loaded successfully
if OPTMOD:
    PSY = OPTMOD.trans(PSY)

# Search through all Invokes and their Schedules for the specified
# Kernel call. Create lists of Invoke names and relative positions
# of ancestor Nodes which contain the specified Kernel call.
INVOKE_NAME = []
ROOT_NODE_POSITION = []
for invoke in PSY.invokes.invoke_list:
    schedule = invoke.schedule
    for kernel in schedule.walk(schedule.children, Kern):
        if kernel.name.lower() == KERNEL_NAME:
            # Root at depth 2 returns the root (ancestor) Node of this
            # Kernel call in the Schedule
            INVOKE_NAME.append(invoke.name)
            ROOT_NODE_POSITION.append(kernel.root_at_depth(2).position)

# Print Invoke name(s) and root Node relative position(s)
if INVOKE_NAME:
    print("\nKernel call '" + KERNEL_NAME +
          "' was found in ")
    for idx, name in enumerate(INVOKE_NAME):
        print("  - " + name + " at root Node position "
              + str(ROOT_NODE_POSITION[idx]))
else:
    print("Kernel call '" + KERNEL_NAME + "' was not found in "
          + ALG_NAME)
