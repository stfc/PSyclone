# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. It adds OpenACC directives to execute
the code on GPUs.
'''

from psyclone.transformations import (ACCParallelTrans, ACCEnterDataTrans,
                                      ACCLoopTrans, ACCRoutineTrans)
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.psyir.nodes import Loop
from psyclone.gocean1p0 import GOKern
from psyclone.psyGen import InvokeSchedule

from fuse_loops import trans as fuse_trans


def trans(psyir):
    '''
    Take the supplied psy object, and fuse the first two loops

    :param psyir: the PSyIR layer to transform.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`


    '''
    # Use existing fuse script to fuse all loops
    fuse_trans(psyir)

    # Module inline all kernels (so they can be modified)
    # Then add an acc routine statement to each of them:
    inline = KernelModuleInlineTrans()
    ktrans = ACCRoutineTrans()
    for kern in psyir.walk(GOKern):
        inline.apply(kern)
        # Put an 'acc routine' directive inside each kernel
        ktrans.apply(kern)

    # Now parallelise each schedule with openacc:
    ptrans = ACCParallelTrans()
    ltrans = ACCLoopTrans()
    dtrans = ACCEnterDataTrans()
    for schedule in psyir.walk(InvokeSchedule):
        # Apply the OpenACC Loop transformation to *every* loop
        # nest in the schedule (which are all outer loops).
        for child in schedule.children:
            TODO1: apply ltrans
            TODO1:  bonus points: when using MPI, there could be
            TODO1:  non-loop statement in the schedule (halo exchange)
            TODO1:  So only apply this if the child is a loop


        # Put all of the loops in a single parallel region
        TODO2: apply ptrans around the whole schedule

        # Add an enter-data directive
        TODO3: apply dtrans to the schedule
