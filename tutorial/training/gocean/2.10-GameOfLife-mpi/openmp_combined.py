# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2026, Science and Technology Facilities Council.
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

'''
Python script intended to be passed to PSyclone via the -s option.
It adds standalone OpenMP loop directives for each outer loop, and then
encloses them all in an OpenMP parallel directive.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.gocean1p0 import GOKern, GOLoop   # noqa: F401
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer
from psyclone.psyir.transformations import OMPParallelTrans
from psyclone.transformations import OMPLoopTrans

from fuse_loops import trans as fuse_trans   # noqa: F401


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object, and add openmp parallel directives
    with individual omp do for the loops of this particular example.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    omp_parallel = OMPParallelTrans()
    # Optional argument: schedule
    omp_do = OMPLoopTrans(omp_schedule="dynamic")
    module_inline = KernelModuleInlineTrans()

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # Module inline all kernels to help gfortran with inlining.
    for kern in schedule.kernels():
        module_inline.apply(kern)

    # Optional:
    # fuse_trans(psyir)

    for loop in schedule.walk(GOLoop):
        if loop.loop_type == "outer":
            omp_do.apply(loop)

    # Look at the schedule before adding 'omp parallel':
    print(schedule.view())

    # TODO: This transformation will fail.
    # How can it be fixed?
    omp_parallel.apply(schedule)

    print(schedule.view())
