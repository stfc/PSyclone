#!/usr/bin/env python
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' PSyclone transformation script showing the introduction of
asynchronous OpenMP GPU directives into Nemo code. '''

from psyclone.psyir.nodes import Loop, Assignment, Directive, Routine
from psyclone.psyir.transformations import ArrayAssignment2LoopsTrans
from psyclone.psyir.transformations import OMPTargetTrans, OMPLoopTrans
from psyclone.psyir.transformations import OMPMinimiseSyncTrans
from psyclone.transformations import TransformationError


def trans(psyir):
    ''' Add OpenMP Target and Loop directives to all loops.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    omp_target_trans = OMPTargetTrans()
    omp_loop_trans = OMPLoopTrans()
    omp_loop_trans.omp_directive = "loop"
    opts = {"nowait": True}

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
                omp_target_trans.apply(loop, options=opts)
                omp_loop_trans.apply(loop, nowait=True)
            except TransformationError:
                # Not all of the loops in the example can be parallelised.
                pass

    for routine in psyir.walk(Routine):
        OMPMinimiseSyncTrans().apply(routine)
