# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone via the -s option.
It adds value range verification for all
kernels.
'''

from psyclone.gocean1p0 import GOLoop
from psyclone.psyir.nodes import FileContainer
from psyclone.psyir.transformations import ValueRangeCheckTrans


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied PSyIR object, and apply the value range change
    transformation to all loops.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    value_range_check = ValueRangeCheckTrans()

    for loop in psyir.walk(GOLoop):
        # Only apply to the outer loop, PSyData will
        # get full arrays provided to check for NANs
        if loop.loop_type == "outer":
            value_range_check.apply(loop)
