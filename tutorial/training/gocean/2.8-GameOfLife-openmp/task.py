# flake8: noqa
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
function via the -s option. It adds kernel extraction code to
all invokes.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.gocean1p0 import GOKern, GOLoop
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import OMPParallelDirective
from psyclone.transformations import (OMPLoopTrans, OMPParallelLoopTrans, 
                                      OMPParallelTrans, OMPTaskloopTrans,
                                      GOceanOMPParallelLoopTrans,
                                      OMPSingleTrans)


from fuse_loops import trans as fuse_trans

def trans(psyir):
    '''
    Take the supplied psyir object, and use omp taskloop directives.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    omp_parallel_loop = GOceanOMPParallelLoopTrans()
    omp_parallel = OMPParallelTrans()
    omp_do = OMPLoopTrans()
    omp_task = OMPTaskloopTrans()
    omp_single = OMPSingleTrans()
    inline = KernelModuleInlineTrans()

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # Inline all kernels to help gfortran with inlining.
    for kern in schedule.kernels():
        inline.apply(kern)

    # We need to have:
    # omp parallel
    # omp single
    # omp taskloop nested in this order
    omp_parallel.apply(schedule)

    # Explicit calls are ugly, results in code like:
    # omp_single.apply(schedule[0].children[0])

    # Easier to understand when using walk:
    for omp_par in schedule.walk(OMPParallelDirective):
        # Apply omp_single to .children[0] of the omp_par directive:

    # Next apply omp_task to all outer loops:
    for loop in schedule.walk(GOLoop):
        if loop.loop_type ...

    print(schedule.view())

