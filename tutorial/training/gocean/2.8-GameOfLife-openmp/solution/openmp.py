# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Python script intended to be passed to PSyclone via the -s option.
It adds module inlining and loop fusion, then adds OpenMP parallelisation
to the code.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.gocean1p0 import GOKern, GOLoop
from psyclone.transformations import OMPParallelLoopTrans
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer

from fuse_loops import trans as fuse_trans  # noqa: F401


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied PSyIR object, apply module inlining and loop fusion.
    Then apply 'omp parallel do' to all loops.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    omp_parallel = OMPParallelLoopTrans(omp_schedule="dynamic")
    omp_parallel.omp_schedule = "static"
    module_inline = KernelModuleInlineTrans()

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # Inline all kernels to help gfortran with inlining.
    for kern in schedule.walk(GOKern):
        module_inline.apply(kern)

    # Optional:
    fuse_trans(psyir)

    for loop in schedule.walk(GOLoop):
        if loop.loop_type == "outer":
            omp_parallel.apply(loop)

    # print(schedule.view())
