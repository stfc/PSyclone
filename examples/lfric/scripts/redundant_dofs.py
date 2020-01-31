# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Laboratory

'''File containing a PSyclone transformation script for the Dynamo0.3
API to apply redundant computation to halo depth 1 for all loops that
iterate over dofs and do not contain a reduction.

'''
from __future__ import absolute_import
from psyclone.transformations import Dynamo0p3RedundantComputationTrans

ITERATION_SPACES = ["dofs"]
DEPTH = 1


def trans(psy):
    '''PSyclone transformation script for the dynamo0.3 API to apply
    redundant computation generically to all loops that iterate over
    dofs, with the exception of loops containing kernels with
    reductions.

    '''
    rc_trans = Dynamo0p3RedundantComputationTrans()

    transformed = 0

    for invoke in psy.invokes.invoke_list:
        schedule = invoke.schedule
        for loop in schedule.loops():
            if loop.iteration_space in ITERATION_SPACES:
                # we may have more than one kernel in the loop so
                # check that none of them are reductions
                reduction = False
                for call in loop.kernels():
                    if call.is_reduction:
                        reduction = True
                        break
                if not reduction:
                    transformed += 1
                    schedule, _ = rc_trans.apply(loop, {"depth": DEPTH})

    print("Transformed {0} loops".format(transformed))
    return psy
