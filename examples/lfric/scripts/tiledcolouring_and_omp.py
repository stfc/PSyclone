# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' File containing a PSyclone transformation script for the LFRic
API to apply tiled-colouring and OpenMP threading.'''

from psyclone.domain.lfric import LFRicConstants
from psyclone.psyir.nodes import Loop, Routine, FileContainer
from psyclone.transformations import LFRicColourTrans, \
    LFRicOMPParallelLoopTrans


def trans(psyir: FileContainer):
    ''' PSyclone transformation script to apply tiled-colouring and OpenMP
    threading.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    ctrans = LFRicColourTrans()
    otrans = LFRicOMPParallelLoopTrans()
    const = LFRicConstants()

    for subroutine in psyir.walk(Routine):

        print(f"Transforming invoke '{subroutine.name}'...")

        # Colour all of the loops over cells (with the tiling option)
        # unless they are on discontinuous spaces
        for child in subroutine.children:
            if (isinstance(child, Loop)
                    and child.field_space.orig_name
                    not in const.VALID_DISCONTINUOUS_NAMES
                    and child.iteration_space.endswith("cell_column")):
                ctrans.apply(child, tiling=True)

        # Then apply OpenMP to each of the colour loop
        for child in subroutine.children:
            if isinstance(child, Loop):
                if child.loop_type == "colours":
                    otrans.apply(child.loop_body[0])
                else:
                    otrans.apply(child)
