# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Python script intended to be passed to PSyclone via the -s option.
It adds OpenMP for an MPI implementation, i.e. it is taking the additional
halo-exchange nodes that are added by PSyclone into account.
'''

from psyclone.gocean1p0 import GOLoop
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer
from psyclone.transformations import GOceanOMPParallelLoopTrans

from copy_kernels_and_fuse_loops import trans as fuse_trans


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object, and apply simple openmp directives.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    omp_parallel_loop = GOceanOMPParallelLoopTrans()

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # Call the existing fuse transformation, which will also module inline.
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
