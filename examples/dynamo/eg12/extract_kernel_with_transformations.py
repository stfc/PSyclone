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
# Modified by R. W. Ford, STFC Daresbury


'''
An example of PSyclone transformation script to extract the specified
Kernel after applying PSyclone transformations to the algorithm code in
gw_mixed_schur_preconditioner_alg_mod.x90. Transformations are applied
via another transformation script which is imported as a Python module.

This script can be applied via the '-s' option when running PSyclone:

$ psyclone -nodm -s extract_kernel_with_transformations.py \
    gw_mixed_schur_preconditioner_alg_mod.x90

Please note that distributed memory is not supported for code extraction
(hence the '-nodm' option above).

The user-specified settings are:
KERNEL_NAME - Name of the Kernel to be extracted as it appears in the
              PSy layer,
INVOKE_NAME - Name of the Invoke containing the Kernel call,
NODE_POSITION - Position of the Node containing the Kernel call.

Names of Invokes and positions of Nodes containing the Kernel call can
be found using the 'find_kernel.py' script.
'''

from __future__ import print_function
from psyclone.transformations import DynamoExtractRegionTrans

# Specify the Kernel name as it appears in the Kernel calls
# (ending with "_code")
KERNEL_NAME = "dg_matrix_vector_kernel_code"
# Specify the name of Invoke containing the Kernel call. If the name
# does not correspond to Invoke names in the Algorithm file no Kernels
# will be extracted.
INVOKE_NAME = "invoke_2"
# Specify the position of the Node containing the Kernel call. If the
# specified Node does not contain the Kernel call nothing will be
# extracted. Please note that if the Invoke contains more than one Kernel
# call with the same name specifying a wrong position may still extract
# the Kernel call, albeit not the one planned originally.
NODE_POSITION = 1


def trans(psy):
    ''' PSyclone transformation script for the Dynamo0.3 API to
    extract the specified Kernel after applying transformations. '''

    # Get instance of the ExtractRegionTrans transformation
    etrans = DynamoExtractRegionTrans()

    # Import transformation script and apply transformations
    import colouring_and_omp as transformation
    psy = transformation.trans(psy)

    # Get Invoke and its Schedule
    invoke = psy.invokes.get(INVOKE_NAME)
    schedule = invoke.schedule

    # Loop over Nodes and check whether they contain the call to
    # the specified Kernel
    for child in schedule.children:
        for kernel in child.coded_kernels():
            # Extract the Node with the specified Kernel call from
            # this Invoke
            if kernel.name.lower() == KERNEL_NAME and \
              child.position == NODE_POSITION:
                print("\nExtracting Node '[" + str(child.position) +
                      "]' with Kernel call '" + KERNEL_NAME +
                      "' from Invoke '" + invoke.name + "'\n")
                schedule, _ = etrans.apply(child)

    # Take a look at the transformed Schedule
    schedule.view()

    return psy
