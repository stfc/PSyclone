#!/usr/bin/env python
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' PSyclone transformation script showing the introduction of OpenMP for GPU
directives into Nemo code. '''

from psyclone.psyir.nodes import Loop, Assignment
from psyclone.psyir.transformations import ArrayAssignment2LoopsTrans
from psyclone.psyir.transformations import OMPTargetTrans, OMPLoopTrans
from psyclone.transformations import TransformationError

USE_GPU = True  # Enable for generating OpenMP target directives

Loop.set_loop_type_inference_rules({
        "lon": {"variable": "ji"},
        "lat": {"variable": "jj"},
        "levels": {"variable": "jk"},
        "tracers": {"variable": "jt"}
})


def trans(psyir):
    ''' Add OpenMP Target and Loop directives to all loops.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    omp_target_trans = OMPTargetTrans()
    omp_loop_trans = OMPLoopTrans()
    omp_loop_trans.omp_directive = "loop"

    # Convert all array implicit loops to explicit loops
    explicit_loops = ArrayAssignment2LoopsTrans()
    for assignment in psyir.walk(Assignment):
        try:
            explicit_loops.apply(assignment)
        except TransformationError:
            pass

    for loop in psyir.walk(Loop):
        if loop.loop_type == "levels":
            try:
                if USE_GPU:
                    omp_target_trans.apply(loop)
                omp_loop_trans.apply(loop)
            except TransformationError:
                # Not all of the loops in the example can be parallelised.
                pass
