# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2023, Science and Technology Facilities Council.
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
from psyclone.transformations import (OMPLoopTrans, OMPParallelLoopTrans, 
                                      OMPParallelTrans,
                                      GOceanOMPParallelLoopTrans)

from fuse_loops import trans as fuse_trans

def trans(psy):
    '''
    Take the supplied psy object, and fuse the first two loops

    :param psy: the PSy layer to transform.
    :type psy: :py:class:`psyclone.psyGen.PSy`

    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.psyGen.PSy`

    '''
    omp_parallel_loop = GOceanOMPParallelLoopTrans()
    omp_parallel = OMPParallelTrans()
    omp_do = OMPLoopTrans()
    inline = KernelModuleInlineTrans()

    invoke = psy.invokes.get("invoke_compute")
    schedule = invoke.schedule

    #omp_parallel_loop.apply(schedule[0])
    #omp_do.apply(schedule[0])
    #omp_parallel.apply(schedule[0:2])
    #schedule.view()

    #return psy

    # Inline all kernels to help gfortran with inlining.
    #for kern in schedule.walk(GOKern):
    #    inline.apply(kern)

    #for loop in schedule.walk(GOLoop):
    #    if loop.loop_type == "outer":
    #        omp_parallel_loop.apply(loop)
    #schedule.view()
    #return psy

    # Inline all kernels to help gfortran with inlining.
    #for kern in schedule.walk(GOKern):
    #    inline.apply(kern)

    fuse_trans(psy)

    # Both ways work - either specify the default in
    # the constructor, or change the schedule
    omp_do_dynamic = OMPLoopTrans(omp_schedule="dynamic")
    omp_do.omp_schedule = "dynamic"
    for loop in schedule.walk(GOLoop):
        if loop.loop_type == "outer":
            omp_do.apply(loop)
    omp_parallel.apply(schedule[1:3])

    schedule.view()
