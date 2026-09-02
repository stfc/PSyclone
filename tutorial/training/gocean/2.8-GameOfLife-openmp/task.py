# flake8: noqa
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Python script intended to be passed to PSyclone via the -s option.
It applies module inlining, and then adds OpenMP task parallelism.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.gocean1p0 import GOKern, GOLoop
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer, OMPParallelDirective
from psyclone.psyir.transformations import OMPParallelTrans
from psyclone.transformations import (GOceanOMPParallelLoopTrans,
                                      OMPLoopTrans, OMPParallelLoopTrans,
                                      OMPSingleTrans, OMPTaskloopTrans)
                                      ,


from copy_kernels_and_fuse_loops import trans as fuse_trans


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object, apply module inlining and then
    add omp taskloop directives.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    omp_parallel_loop = GOceanOMPParallelLoopTrans()
    omp_parallel = OMPParallelTrans()
    omp_do = OMPLoopTrans()
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

    # Explicit calls are ugly, results in code like:
    # omp_single.apply(schedule[0].children[0])

    # Easier to understand when using walk:
    for omp_par in schedule.walk(OMPParallelDirective):
        # Apply omp_single to .children[0] of the omp_par directive:

    # Next apply omp_task to all outer loops:
    for loop in schedule.walk(GOLoop):
        if loop.loop_type ...

    print(schedule.view())

