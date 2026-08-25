#!/usr/bin/env python
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' PSyclone transformation script showing the introduction of OpenMP
directives into Nemo code. '''

from psyclone.psyGen import TransInfo
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.nodes import Loop

# Set up some loop_type inference rules in order to reference useful domain
# loop constructs by name
Loop.set_loop_type_inference_rules({
        "lon": {"variable": "ji"},
        "lat": {"variable": "jj"},
        "levels": {"variable": "jk"},
        "tracers": {"variable": "jt"}
})


def trans(psyir):
    ''' Add OpenMP Parallel Loop directives to Nemo loops over levels.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    omp_trans = TransInfo().get_trans_name('OMPParallelLoopTrans')

    for loop in psyir.walk(Loop):
        if loop.loop_type == "levels":
            try:
                omp_trans.apply(loop)
            except TransformationError:
                # Not all of the loops in the example can be parallelised.
                pass
