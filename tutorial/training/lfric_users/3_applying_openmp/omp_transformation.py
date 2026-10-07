# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
This script applies OpenMP parallelisation to each loop.
'''

from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer, Loop
from psyclone.transformations import LFRicOMPParallelLoopTrans


def trans(psyir: FileContainer) -> None:
    """
    PSyclone transformation script for the LFRic api to apply
    OpenMP parallel to all loops.

    :param psyir: the PSyIR of the PSy-layer.
    """

    otrans = LFRicOMPParallelLoopTrans()

    # Loop over all of the Invokes in the PSy object
    for invoke in psyir.walk(InvokeSchedule):
        print(f"Transforming invoke '{invoke.name}':")
        # Apply OpenMP to each of the loops
        for loop in invoke.walk(Loop):
            otrans.apply(loop)
