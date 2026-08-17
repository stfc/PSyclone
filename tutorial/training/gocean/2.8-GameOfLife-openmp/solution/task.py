# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Python script intended to be passed to PSyclone via the -s option.
It applies kernel module inlining, and then adds OpenMP taskloop
directives
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.gocean1p0 import GOLoop
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer, OMPParallelDirective
from psyclone.psyir.transformations import OMPParallelTrans, OMPTaskloopTrans
from psyclone.transformations import OMPSingleTrans


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object, apply module inlining and then
    add omp taskloop directives.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    omp_parallel = OMPParallelTrans()
    omp_task = OMPTaskloopTrans()
    omp_single = OMPSingleTrans()
    module_inline = KernelModuleInlineTrans()

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # Inline all kernels to help gfortran with inlining.
    for kern in schedule.kernels():
        module_inline.apply(kern)

    # We need to have:
    # omp parallel
    # omp single
    # omp taskloop nested in this order
    omp_parallel.apply(schedule)

    for omp_par in schedule.walk(OMPParallelDirective):
        omp_single.apply(omp_par.children[0])
    for loop in schedule.walk(GOLoop):
        if loop.loop_type == "outer":
            omp_task.apply(loop)

    # print(schedule.view())
