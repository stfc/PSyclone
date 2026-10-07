# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''File containing a PSyclone transformation script for the LFRic
API to apply OpenMP Parallel Loop parallelisation. This script can be
applied via the -s option in the psyclone command, it is not designed
to be directly run from python.

'''
from psyclone.psyir.transformations import OMPParallelTrans
from psyclone.transformations import LFRicOMPParallelLoopTrans, \
    TransformationError, LFRicColourTrans, \
    LFRicOMPLoopTrans
from psyclone.psyGen import Loop
from psyclone.domain.lfric.function_space import FunctionSpace


def trans(psyir):
    '''PSyclone transformation script for the LFRic API that applies
    OpenMP parallel loop parallelisation. It also outputs a textual
    representation of the transformed PSyIR.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    otrans = LFRicOMPParallelLoopTrans()

    # Add OpenMP parallel do directives to the loops
    for loop in psyir.loops():
        try:
            otrans.apply(loop)
        except TransformationError as info:
            print(str(info.value))

    # take a look at what we've done
    print(psyir.view())
