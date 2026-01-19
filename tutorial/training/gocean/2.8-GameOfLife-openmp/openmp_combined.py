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

'''
Python script intended to be passed to PSyclone via the -s option.
It applies module inlining, then adds 'omp do' to all loops and then
adds an outer `omp parallel`
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.gocean1p0 import GOKern, GOLoop
from psyclone.transformations import OMPLoopTrans, OMPParallelTrans
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer

from fuse_loops import trans as fuse_trans


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied PSyIR object, and apply module inlining. Then add
    'omp do' to all loops, and add an outer `omp parallel`.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    omp_parallel = OMPParallelTrans()
    omp_do = OMPLoopTrans()
    module_inline = KernelModuleInlineTrans()

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # Inline all kernels to help gfortran with inlining.
    for kern in schedule.walk(GOKern):
        module_inline.apply(kern)

    # TODO (later): Try changing the schedule to be dynamic. This can
    # either be done at the constructor above, or assigning to the
    # omp_schedule attribute of the omp_do transformation

    # TODO (later): Apply the loop fusion transformation (already
    # imported above)

    # TODO: Apply OpenMP do around all outer loops:

    # TODO: Now add the OMP PARALLEL around all loops
    # by applying the transformation to the schedule
    # (or you could keep a list of all loops that you
    # have applied OpenMP do to and provide this list)

    print(schedule.view())
