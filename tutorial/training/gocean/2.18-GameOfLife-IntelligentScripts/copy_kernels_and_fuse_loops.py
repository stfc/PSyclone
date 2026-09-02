# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Python script intended to be passed to PSyclone via the -s option.
It should fuse all possible loops in all invokes.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans
from psyclone.gocean1p0 import GOKern, GOLoop   # noqa: F401
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer
from psyclone.psyir.transformations import TransformationError   # noqa: F401


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object, apply kernel inlining and fuse all loops.

    :param psyir: the PSyIR of the PSy-layer.

    '''

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # Inline all kernels to help gfortran with inlining.
    module_inline = KernelModuleInlineTrans()
    for kern in schedule.walk(GOKern):
        module_inline.apply(kern)

    fuse = GOceanLoopFuseTrans()   # noqa: F841
    # Do something intelligent here :)
