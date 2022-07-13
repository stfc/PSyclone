# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#          A. B. G. Chalk, STFC Daresbury Lab

'''A simple test script showing the introduction of OpenMP tasking with
PSyclone.

'''

from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans
from psyclone.transformations import (ACCParallelTrans, ACCEnterDataTrans, 
                                      ACCLoopTrans, ACCRoutineTrans,
                                      KernelModuleInlineTrans)
from psyclone.psyir.transformations import LoopFuseTrans
from psyclone.psyir.nodes import Loop


def trans(psy):
    '''
    Transformation routine for use with PSyclone. Applies the OpenMP
    taskloop and taskwait transformations to the PSy layer.

    :param psy: the PSy object which this script will transform.
    :type psy: :py:class:`psyclone.psyGen.PSy`
    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.psyGen.PSy`

    '''

    fuse = LoopFuseTrans()
    PTRANS = ACCParallelTrans()
    DTRANS = ACCEnterDataTrans()
    LTRANS = ACCLoopTrans()
    ktrans = ACCRoutineTrans()
    itrans = KernelModuleInlineTrans()

    schedule = psy.invokes.get('invoke_2').schedule
    # fuse all outer loops
    fuse.apply(schedule.children[0],
               schedule.children[1])
    fuse.apply(schedule.children[0],
               schedule.children[1])
    fuse.apply(schedule.children[0],
               schedule.children[1])
    print(schedule.view())

    # fuse all inner loops
    fuse.apply(schedule.children[0].loop_body[0],
               schedule.children[0].loop_body[1])
    fuse.apply(schedule.children[0].loop_body[0],
               schedule.children[0].loop_body[1])
    fuse.apply(schedule.children[0].loop_body[0],
               schedule.children[0].loop_body[1])

    LTRANS.apply(schedule.children[0], {"collapse": 2})

    # Create an OpenACC parallel region around the loop
    PTRANS.apply(schedule.children[0])
    print(schedule.view())

    # Add an OpenACC enter-data directive
    DTRANS.apply(schedule)

    #print(fwriter(PSY.container))
    for kern in schedule.coded_kernels():
        print("KERN", kern.name)
        #if kern.name == "compute_cu_code":
        #    g2localtrans.apply(kern)
        ktrans.apply(kern)
        itrans.apply(kern)

