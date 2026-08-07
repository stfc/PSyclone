# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


''' File containing a PSyclone transformation script for the LFRic
API to apply colouring and OpenMP generically. This can be applied via
the -s option in the "psyclone" script. '''
from psyclone.transformations import LFRicColourTrans, \
    LFRicOMPParallelLoopTrans
from psyclone.psyir.nodes import Loop
from psyclone.psyGen import InvokeSchedule
from psyclone.domain.lfric import LFRicConstants


def trans(psyir):
    ''' PSyclone transformation script for the LFRic api to apply
    colouring and OpenMP generically.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    ctrans = LFRicColourTrans()
    otrans = LFRicOMPParallelLoopTrans()
    const = LFRicConstants()

    # Loop over all of the Invokes
    for subroutine in psyir.walk(InvokeSchedule):
        print(f"Transforming invoke '{subroutine.name}'...")

        # Colour all of the loops over cells unless they are on
        # discontinuous spaces
        for child in subroutine.children:
            if isinstance(child, Loop) \
               and child.field_space.orig_name \
               not in const.VALID_DISCONTINUOUS_NAMES \
               and child.iteration_space.endswith("cell_column"):
                ctrans.apply(child)
        # Then apply OpenMP to each of the colour loops
        for child in subroutine.children:
            if isinstance(child, Loop):
                if child.loop_type == "colours":
                    otrans.apply(child.loop_body[0])
                else:
                    otrans.apply(child)
