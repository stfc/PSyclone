# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2025, Science and Technology Facilities Council.
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
# Author: A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified: R. W. Ford, STFC Daresbury Lab

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. Transforms all kernels in the invoke
to have them compiled for an OpenACC accelerator. '''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.psyir.nodes import Loop
from psyclone.transformations import (
    ACCParallelTrans, ACCEnterDataTrans, ACCLoopTrans, ACCRoutineTrans,
    TransformationError)


def trans(psyir):
    ''' Apply OpenACC transformations to the invoke_0 subroutine

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    ptrans = ACCParallelTrans()
    ltrans = ACCLoopTrans()
    dtrans = ACCEnterDataTrans()
    ktrans = ACCRoutineTrans()
    itrans = KernelModuleInlineTrans()

    for schedule in psyir.children[0].children:
        if schedule.name == 'invoke_0_inc_field':

            # Put an 'acc routine' directive inside each kernel
            try:
                for kern in schedule.coded_kernels():
                    ktrans.apply(kern)
                    itrans.apply(kern)
            except TransformationError:
                # TODO #2856: Currenly we refuse to offload code containing
                # the REAL intrinsic
                continue

            # Apply the OpenACC Loop transformation to *every* loop
            # nest in the schedule
            for child in schedule.children:
                if isinstance(child, Loop):
                    ltrans.apply(child, {"collapse": 2})

            # Put all of the loops in a single parallel region
            ptrans.apply(schedule.children)

            # Add an enter-data directive
            dtrans.apply(schedule)
