# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2025, Science and Technology Facilities Council
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
# Authors: R. W. Ford, N. Nobre and S. Siso, STFC Daresbury Lab

'''File containing a PSyclone transformation script for the LFRic
API to apply redundant computation to halo depth 1 for all instances
of loops that iterate over dofs and contain the setval_c builtin.

'''
from psyclone.transformations import LFRicRedundantComputationTrans

# We don't include "owned_dofs" here as we only want loops that
# are permitted to perform redundant computation.
ITERATION_SPACES = ["dofs"]
KERNEL_NAMES = ["setval_c"]
DEPTH = 1


def trans(psyir):
    '''PSyclone transformation script for the lfric API to apply
    redundant computation into the level 1 halo generically to all
    loops that iterate over dofs and exclusively contain the setval_c
    builtin. The reason for choosing this particular builtin is that
    this builtin only writes to data so will not cause any additional
    halo exchanges, or increases in halo exchange depth, through
    redundant computation.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    rc_trans = LFRicRedundantComputationTrans()

    transformed = 0

    for loop in psyir.loops():
        if loop.iteration_space in ITERATION_SPACES:
            # we may have more than one kernel in the loop so
            # check that all of them are in the list of accepted
            # kernel names
            for call in loop.kernels():
                if call.name not in KERNEL_NAMES:
                    break
            else:
                # All kernels are valid
                transformed += 1
                rc_trans.apply(loop, {"depth": DEPTH})

    print(f"Transformed {transformed} loops")
