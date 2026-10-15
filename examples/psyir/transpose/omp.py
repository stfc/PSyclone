# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

from psyclone.psyir.transformations import OMPLoopTrans
from psyclone.psyir.nodes import Loop, Routine


# Find the first loop in the subroutine "trans" (it is a doubly nested
# loop), and then parallelise it using OpenMP.
def trans(psyir):
    for routine in psyir.walk(Routine):
        if routine.name == "trans":
            loop = routine.walk(Loop)[0]
            OMPLoopTrans(omp_directive="paralleldo").apply(loop, collapse=2)
