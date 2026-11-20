# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Python script intended to be passed to PSyclone via the -s option.
It adds 'omp do' to all loops and adds an outer `omp parallel`.
'''

from psyclone.gocean1p0 import GOLoop
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer
from psyclone.psyir.transformations import OMPParallelTrans
from psyclone.transformations import OMPLoopTrans


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied PSyIR object and apply 'omp do' to all loops and
    add an outer `omp parallel`.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    omp_parallel = OMPParallelTrans()
    # Optional argument: schedule
    omp_do = OMPLoopTrans(omp_schedule="dynamic")

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # Both ways work - either specify the default in
    # the constructor, or change the schedule here:
    omp_do.omp_schedule = "static"
    for loop in schedule.walk(GOLoop):
        if loop.loop_type == "outer":
            omp_do.apply(loop)

    # Now add the OMP PARALLEL around all loops:
    omp_parallel.apply(schedule)

    # print(schedule.view())
