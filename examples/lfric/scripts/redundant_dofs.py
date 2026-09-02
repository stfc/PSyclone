# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''File containing a PSyclone transformation script for the LFRic
API to apply redundant computation to halo depth 1 for all loops that
iterate over dofs and do not contain a reduction.

'''
from psyclone.domain.lfric.transformations import (
    LFRicRedundantComputationTrans)

ITERATION_SPACES = ["dofs"]
DEPTH = 1


def trans(psyir):
    '''PSyclone transformation script for the lfric API to apply
    redundant computation generically to all loops that iterate over
    dofs, with the exception of loops containing kernels with
    reductions and those that are restricted to owned dofs only.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    rc_trans = LFRicRedundantComputationTrans()

    transformed = 0

    for loop in psyir.loops():
        if loop.iteration_space in ITERATION_SPACES:
            # we may have more than one kernel in the loop so
            # check that none of them are reductions
            for call in loop.kernels():
                if call.is_reduction:
                    break
            else:
                # No reduction found
                transformed += 1
                rc_trans.apply(loop, {"depth": DEPTH})

    print(f"Transformed {transformed} loops")
