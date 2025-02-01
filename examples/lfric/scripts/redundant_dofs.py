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
API to apply redundant computation to halo depth 1 for all loops that
iterate over dofs and do not contain a reduction.

'''
from psyclone.transformations import Dynamo0p3RedundantComputationTrans

ITERATION_SPACES = ["dofs"]
DEPTH = 1


def trans(psyir):
    '''PSyclone transformation script for the lfric API to apply
    redundant computation generically to all loops that iterate over
    dofs, with the exception of loops containing kernels with
    reductions.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    rc_trans = Dynamo0p3RedundantComputationTrans()

    transformed = 0

    for loop in psyir.loops():
        if loop.iteration_space in ITERATION_SPACES:
            # we may have more than one kernel in the loop so
            # check that none of them are reductions
            for call in loop.kernels():
                if call.is_reduction:
                    break
            else:
                # No reduction found
                transformed += 1
                rc_trans.apply(loop, {"depth": DEPTH})

    print(f"Transformed {transformed} loops")
