# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""
This script applies colouring if required, and then OpenMP parallelisation
to each loop.
"""

from psyclone.domain.lfric import LFRicConstants
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer, Loop
from psyclone.transformations import (LFRicOMPParallelLoopTrans,
                                      LFRicColourTrans)


def trans(psyir: FileContainer) -> None:
    """
    PSyclone transformation script for the LFRic API to apply
    colouring if required, and then OpenMP parallel to all loops.

    :param psyir: the PSyIR of the PSy-layer.
    """

    otrans = LFRicOMPParallelLoopTrans()
    const = LFRicConstants()
    colour_trans = LFRicColourTrans()

    # Loop over all of the Invokes in the PSy object to see if
    # colouring needs to be applied:
    for schedule in psyir.walk(InvokeSchedule):
        print(f"Transforming invoke '{schedule.name}':")
        for loop in schedule.walk(Loop):
            if loop.field_space.orig_name \
                    not in const.VALID_DISCONTINUOUS_NAMES \
                    and loop.iteration_space == "cell_column":
                colour_trans.apply(loop)

        # Check all outer loop - if there is a coloured loop,
        # its inner loop must be omp-parallelised:
        for child in schedule.children:
            if isinstance(child, Loop):
                if child.loop_type == "colours":
                    otrans.apply(child.loop_body[0])
                else:
                    otrans.apply(child)
