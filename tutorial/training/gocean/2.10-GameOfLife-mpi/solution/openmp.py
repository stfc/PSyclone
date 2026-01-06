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
Python script intended to be passed to PSyclone's generate()
function via the -s option. It adds openmp for an MPI implementation,
i.e. it is taking the additional halo-exchange nodes that are added
by PSyclone into account.
'''

from psyclone.gocean1p0 import GOLoop
from psyclone.psyGen import InvokeSchedule
from psyclone.transformations import GOceanOMPParallelLoopTrans

from fuse_loops import trans as fuse_trans


def trans(psyir):
    '''
    Take the supplied psyir object, and apply simple openmp directives.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    omp_parallel_loop = GOceanOMPParallelLoopTrans()

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # Call the existing fuse transformation, which will also inline.
    fuse_trans(psyir)

    # As example, select schedule dynamic. Both ways work - either specify
    # the default in the constructor above, or change the schedule
    omp_parallel_loop.omp_schedule = "dynamic"
    for loop in schedule.walk(GOLoop):
        # We only apply OpenMP to the outer loop. The OpenMP collapse
        # parameter could also be used (though might not be ideal
        # performance-wise in this example).
        if loop.loop_type == "outer":
            omp_parallel_loop.apply(loop)

    # print(schedule.view())
