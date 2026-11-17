# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''File containing a PSyclone transformation script for the LFRic
API to apply redundant computation to halo depth 1 for all instances
of loops that iterate over dofs and contain the setval_c builtin.

'''
from psyclone.domain.lfric.transformations import (
    LFRicRedundantComputationTrans)

# We don't include "owned_dofs" here as we only want loops that
# are permitted to perform redundant computation.
ITERATION_SPACES = ["dofs"]
KERNEL_NAMES = ["setval_c"]
DEPTH = 1


def trans(psyir):
    '''PSyclone transformation script for the lfric API to apply
    redundant computation into the level 1 halo generically to all
    loops that iterate over dofs and exclusively contain the setval_c
    builtin. The reason for choosing this particular builtin is that
    this builtin only writes to data so will not cause any additional
    halo exchanges, or increases in halo exchange depth, through
    redundant computation.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    rc_trans = LFRicRedundantComputationTrans()

    transformed = 0

    for loop in psyir.loops():
        if loop.iteration_space in ITERATION_SPACES:
            # we may have more than one kernel in the loop so
            # check that all of them are in the list of accepted
            # kernel names
            for call in loop.kernels():
                if call.name not in KERNEL_NAMES:
                    break
            else:
                # All kernels are valid
                transformed += 1
                rc_trans.apply(loop, {"depth": DEPTH})

    print(f"Transformed {transformed} loops")
