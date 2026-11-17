#!/usr/bin/env python
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' PSyclone transformation script showing the introduction of
asynchronous OpenMP directives into Nemo code. '''

from psyclone.psyir.transformations import (
        ArrayAssignment2LoopsTrans,
        OMPLoopTrans,
        OMPMinimiseSyncTrans,
        TransformationError,
        MaximalOMPParallelRegionTrans
)
from psyclone.psyir.nodes import (
        Assignment,
        Directive,
        Loop,
        Routine,
)


def trans(psyir):
    ''' Adds OpenMP Loop directives with nowait to Nemo loops over levels.
    This is followed by applying OpenMP parallel directives as required
    with the OMPMaximalParallelRegionTrans, before removing barriers where
    possible.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    loop_trans = OMPLoopTrans()
    minsync_trans = OMPMinimiseSyncTrans()

    # First convert assignments to loops whenever possible
    for assignment in psyir.walk(Assignment):
        try:
            ArrayAssignment2LoopsTrans().apply(assignment)
        except TransformationError:
            pass

    # Apply loop_trans to all the loops possible.
    for loop in psyir.walk(Loop):
        if not loop.ancestor(Directive):
            try:
                loop_trans.apply(loop, nowait=True)
            except TransformationError:
                # Not all of the loops in the example can be parallelised.
                pass

    # Apply the largest possible parallel regions and remove any barriers that
    # can be removed.
    for routine in psyir.walk(Routine):
        MaximalOMPParallelRegionTrans().apply(routine)
        minsync_trans.apply(routine)
