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
extract the specified Kernel from multiple Invokes.
This script can be applied via the -s option when running PSyclone:

$ psyclone -nodm -s extract_kernel_multi_invokes.py alg_mod.x90

Please note that distributed memory is not supported for code extraction
(hence the '-nodm' option above).

The user-specified settings are:
KERNEL_NAME - Name of the Kernel to be extracted as it appears in the
              PSy layer,
INVOKE_NAMES - list of the names of Invokes containing the Kernel call.

This information can be returned by the 'find_kernel.py' script.

Please note that if there are multiple instances of the same Kernel call
in an Invoke all of them will be extracted. This can be prevented by
specifying the relative position of the Kernel's ancestor Node in the
'Extractor.extract_kernel' call (position can  also be returned by the
'find_kernel.py' script).
'''

from __future__ import print_function
from psyclone.extractor import Extractor

# Specify the Kernel name as it appears in the Kernel calls
# (ending with "_code")
KERNEL_NAME = "matrix_vector_code"
# Specify the list of Invoke names containing the Kernel calls. If names
# in the list do not correspond to PSy Invoke names in the Algorithm file
# no Kernels will be extracted. If there is no specified Kernel call in
# PSy Invokes the 'Extractor.extract_kernel' function will exit with
# an error.
INVOKE_NAMES = ["invoke_0", "invoke_4"]


def trans(psy):
    ''' PSyclone transformation script for the Dynamo0p3 API to
    extract the specified Kernel. '''

    # Loop over all of the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:

        # Extract the specified Kernel call from selected Invokes
        if invoke.name in INVOKE_NAMES:

            print("\nExtracting Kernel '" + KERNEL_NAME + "' from "
                  "Invoke '" + invoke.name + "'")
            schedule = invoke.schedule

            schedule = Extractor.extract_kernel(schedule, KERNEL_NAME)
            schedule.view()
            invoke.schedule = schedule

    return psy
