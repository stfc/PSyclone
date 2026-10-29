# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''File containing a PSyclone transformation script for the LFRic
API to apply colouring and then OpenMP parallelisation to an
invoke. This script can be applied via the -s option in the psyclone
command.

'''
from psyclone.psyir.transformations import OMPParallelTrans
from psyclone.transformations import LFRicOMPParallelLoopTrans, \
    TransformationError, LFRicColourTrans, \
    LFRicOMPLoopTrans
from psyclone.psyGen import Loop
from psyclone.domain.lfric import LFRicConstants


def trans(psyir):
    '''PSyclone transformation script for the LFRic API that applies
    loop colouring and OpenMP parallel loop parallelisation. It also
    outputs a textual representation of the transformed PSyIR.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    otrans = LFRicOMPParallelLoopTrans()
    ctrans = LFRicColourTrans()
    ptrans = OMPParallelTrans()
    ltrans = LFRicOMPLoopTrans()
    const = LFRicConstants()

    # Colour any loops that need colouring
    for loop in psyir.walk(Loop):
        if (loop.field_space.orig_name not in
                const.VALID_DISCONTINUOUS_NAMES and
                loop.iteration_space == "cell_column"):
            ctrans.apply(loop)

    # Add OpenMP parallel do directives to the loops
    for loop in psyir.walk(Loop):
        try:
            # Make sure reductions are reproducible
            if loop.reductions():
                ptrans.apply(loop)
                ltrans.apply(loop, {"reprod": True})
            else:
                otrans.apply(loop)
        except TransformationError as info:
            print(str(info.value))

    # take a look at what we've done
    print(psyir.view())
