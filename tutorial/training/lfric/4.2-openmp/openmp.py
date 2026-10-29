# flake8: noqa
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Python script intended to be passed to PSyclone via the -s option.
It adds a generic OpenMP parallelisation to the code.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.lfric import LFRicKern, LFRicLoop
from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans
from psyclone.psyir.nodes import FileContainer
from psyclone.psyir.transformations import OMPParallelTrans
from psyclone.transformations import OMPLoopTrans, TransformationError


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object, apply module inlining and add generic
    OpenMP parallelisation to the code. Also check if loop fusion can
    be applied.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    omp_parallel = OMPParallelTrans()
    omp_loop = OMPLoopTrans()
    module_inline = KernelModuleInlineTrans()

    for invoke in psyir.walk(InvokeSchedule):
        schedule = invoke.schedule

        # Module inline all kernels to help with inlining.
        for kern in schedule.walk(LFRicKern):
            module_inline.apply(kern)

        all_loops = list(schedule.walk(LFRicLoop))
        # Can we use loop fusion? Note that we need to use the specific
        # LFRic fusion (since in the LFRic API we potentially know the
        # function space on which the fields are defined, while in general
        # we don't know this).
        fuse = LFRicLoopFuseTrans()
        # Try loop fuse here

        # Now add OpenMP parallel do
