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
File containing a PSyclone transformation script for the Dynamo0p3 API to
extract the single specified Kernel (there may be multiple instances of
the same Kernel call in an Invoke).
This script can be applied via the -s option when running PSyclone:

$ psyclone -nodm -s extract_single_kernel.py alg_mod.x90

Please note that distributed memory is not supported for code extraction
(hence the '-nodm' option above).

The user-specified settings are:
KERNEL_NAME - Name of the Kernel to be extracted as it appears in the
              PSy layer,
ROOT_NODE_POSITION - relative position of ancestor Node containing the
                     specified Kernel call in the Invoke Schedule,
INVOKE_NAME - name of the Invoke containing the Kernel call.

This information can be returned by the 'find_kernel.py' script.
'''

from __future__ import print_function
from psyclone.extractor import Extractor

# Specify the Kernel name as it appears in the Kernel calls
# (ending with "_code")
KERNEL_NAME = "dg_matrix_vector_code"
# Specify the relative position of the root (ancestor) Node of this
# Kernel call in the Schedule (otherwise all Kernel calls with the
# same name in an Invoke will be extracted)
ROOT_NODE_POSITION = 1
# Specify the name of the Invoke containing the Kernel call. If the
# name does not correspond to PSy Invoke names in the Algorithm file
# no Kernel will be extracted. If there is no specified Kernel call
# in PSy Invokes the 'Extractor.extract_kernel' function will exit
# with an error.
INVOKE_NAME = "invoke_5"


def trans(psy):
    ''' PSyclone transformation script for the Dynamo0p3 API to
    extract the specified Kernel. '''

    # Loop over all of the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:

        # Extract the specified Kernel call from the selected Invoke
        if invoke.name == INVOKE_NAME:

            print("\nExtracting Kernel '" + KERNEL_NAME + "' from "
                  "Invoke '" + invoke.name + "'")
            schedule = invoke.schedule

            schedule = Extractor.extract_kernel(schedule, KERNEL_NAME,
                                                ROOT_NODE_POSITION)
            schedule.view()
            invoke.schedule = schedule

    return psy
