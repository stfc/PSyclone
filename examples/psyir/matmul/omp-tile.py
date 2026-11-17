# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

from psyclone.psyir.transformations import LoopTilingTrans
from psyclone.psyir.transformations import OMPLoopTrans
from psyclone.psyir.nodes import Loop, Routine


# Find the first loop in the subroutine "my_matmul" (it is a triply nested
# loop), then parallelise the two outer loops using OpenMP, and then apply 3D
# loop tiling.
def trans(psyir):
    for routine in psyir.walk(Routine):
        if routine.name == "my_matmul":
            loop = routine.walk(Loop)[0]
            OMPLoopTrans(omp_directive="paralleldo").apply(loop, collapse=2)
            LoopTilingTrans().apply(loop, tiledims=[8, 8, 8])
