# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council
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
# Author: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''File containing a PSyclone transformation script for the Dynamo0p3
API to apply the Kernels directive to all loops generically. This can
be applied via the -s option in the psyclone command, it is not
designed to be directly run from python.

'''
from __future__ import print_function
from psyclone.transformations import ACCEnterDataTrans, \
    ACCLoopTrans, ACCRoutineTrans, Dynamo0p3ColourTrans, ACCKernelsTrans
from psyclone.domain.lfric.function_space import FunctionSpace


def trans(psy):
    '''PSyclone transformation script for the dynamo0p3 api to apply
    OpenACC Kernels directives to all loops generically. It also
    outputs a textual representation of the transformated PSyIR.

    :param psy: a PSyclone PSy object which captures the algorithm and \
        kernel information required by PSyclone.
    :type psy: subclass of :py:class:`psyclone.psyGen.PSy`

    '''
    kernels_trans = ACCKernelsTrans()

    # Loop over all of the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:

        schedule = invoke.schedule

        # Apply kernels directives to any loop nodes that are
        # children of the schedule node.
        for loop in schedule.loops():
            kernels_trans.apply([loop])

        print(schedule.view())

    return psy
