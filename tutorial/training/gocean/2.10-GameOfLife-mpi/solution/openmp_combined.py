# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Python script intended to be passed to PSyclone via the -s option.
It adds standalone OpenMP loop directives for each outer loop, and then
encloses them all in an OpenMP parallel directive.
'''

from psyclone.gocean1p0 import GOLoop
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer
from psyclone.psyir.transformations import OMPParallelTrans
from psyclone.transformations import OMPLoopTrans

# pylint: disable=unused-import
from copy_kernels_and_fuse_loops import trans as fuse_trans   # noqa: F401


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object, and add openmp parallel directives
    with individual omp do for the loops of this particular example.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    omp_parallel = OMPParallelTrans()
    # Optional argument: schedule
    omp_do = OMPLoopTrans(omp_schedule="dynamic")

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # Bring all kernels into the same scope to help gfortran with inlining
    # and fuse loops.
    fuse_trans(psyir)

    for loop in schedule.walk(GOLoop):
        if loop.loop_type == "outer":
            omp_do.apply(loop)

    # Look at the schedule before adding 'omp parallel':
    # print(schedule.view())

    # Now add the OMP PARALLEL around all loops. In case of
    # distributed memory the first node is the halo exchange,
    # which must be excluded:
    omp_parallel.apply(schedule[1:])

    # print(schedule.view())
