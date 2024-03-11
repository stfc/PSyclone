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
API to apply OpenACC Loop, Parallel and Enter Data directives
generically. This can be applied via the -s option in the psyclone
command, it is not designed to be directly run from python.

'''
from __future__ import print_function
from psyclone.transformations import ACCEnterDataTrans, \
    ACCLoopTrans, ACCRoutineTrans, Dynamo0p3ColourTrans, ACCKernelsTrans
from psyclone.domain.lfric import LFRicConstants


def trans(psy):
    '''PSyclone transformation script for the dynamo0p3 api to apply
    OpenACC loop, parallel and enter data directives generically.

    :param psy: a PSyclone PSy object which captures the algorithm and \
        kernel information required by PSyclone.
    :type psy: subclass of :py:class:`psyclone.psyGen.PSy`

    '''
    kernels_trans = ACCKernelsTrans()
    routine_trans = ACCRoutineTrans()
    ctrans = Dynamo0p3ColourTrans()
    loop_trans = ACCLoopTrans()
    enter_trans = ACCEnterDataTrans()
    const = LFRicConstants()

    # Loop over all of the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:

        schedule = invoke.schedule

        # Colour loops as required
        for loop in schedule.loops():
            if loop.field_space.orig_name \
               not in const.VALID_DISCONTINUOUS_NAMES \
               and loop.iteration_space == "cell_column":
                ctrans.apply(loop)

        # Add Kernels and Loop directives
        for loop in schedule.loops():
            if loop.loop_type != "colours":
                kernels_trans.apply([loop])
                loop_trans.apply(loop)

        # Add Routine directive to kernels
        for kernel in schedule.coded_kernels():
            routine_trans.apply(kernel)

        # Add Enter Data directive covering all of the PSy layer.
        enter_trans.apply(schedule)

        print(schedule.view())

    return psy
