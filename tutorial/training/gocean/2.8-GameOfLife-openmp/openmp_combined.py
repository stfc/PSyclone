# flake8: noqa
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Python script intended to be passed to PSyclone via the -s option.
It applies module inlining, then adds 'omp do' to all loops and then
adds an outer `omp parallel`
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.gocean1p0 import GOKern, GOLoop
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer
from psyclone.psyir.transformations import OMPParallelTrans
from psyclone.transformations import OMPLoopTrans

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
