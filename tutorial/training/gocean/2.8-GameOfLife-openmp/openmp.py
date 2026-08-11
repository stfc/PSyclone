# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Python script intended to be passed to PSyclone via the -s option.
It applies module inlinind and then adds a generic OpenMP parallelisation
to the code.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.gocean1p0 import GOKern, GOLoop   # noqa: F401
from psyclone.transformations import OMPParallelLoopTrans
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object, apply module inlining, and then\
    apply 'omp parallel do' to all loops.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    omp_parallel = OMPParallelLoopTrans()   # noqa: F841
    module_inline = KernelModuleInlineTrans()

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # Inline all kernels to help gfortran with inlining.
    for kern in schedule.walk(GOKern):
        module_inline.apply(kern)

    # TODO: apply omp_parallel to all outer loops:

    # Check the expected output
    print(schedule.view())
