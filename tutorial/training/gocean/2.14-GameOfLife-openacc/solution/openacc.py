# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2025, Science and Technology Facilities Council.
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

'''Python script intended to be passed to PSyclone's via the -s option.
It adds OpenACC directives to execute the code on GPUs.
'''

from psyclone.transformations import (ACCParallelTrans, ACCEnterDataTrans,
                                      ACCLoopTrans, ACCRoutineTrans)
from psyclone.psyir.nodes import Loop
from psyclone.gocean1p0 import GOKern
from psyclone.psyGen import InvokeSchedule

from fuse_loops import trans as fuse_trans


def trans(psyir):
    '''
    Take the supplied psyir object, use the existing fuse_loops
    script to do module inlining and fuse the first three loops,
    then apply OpenACC directives.

    :param psyir: the PSyIR layer to transform.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    # Use existing fuse script to fuse all loops
    fuse_trans(psyir)

    # Module inline all kernels (so they can be modified)
    # Then add an acc routine statement to each of them:
    ktrans = ACCRoutineTrans()
    for kern in psyir.walk(GOKern):
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
            if isinstance(child, Loop):
                ltrans.apply(child, {"collapse": 2})

        # Put all of the loops in a single parallel region
        ptrans.apply(schedule)

        # Add an enter-data directive
        dtrans.apply(schedule)
