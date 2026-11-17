# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

from psyclone.psyir.transformations import OMPLoopTrans
from psyclone.psyir.nodes import Loop, Routine, UnknownDirective


def trans(psyir):
    """Find the custom directive and add the associated loop to the list of
    loops to skip. Parallelise the rest"""
    for routine in psyir.walk(Routine):
        psy_dirs = routine.walk(UnknownDirective)
        loops_to_skip = []
        for psy_dir in psy_dirs:
            if psy_dir.directive_string == "my_dir no_par":
                position = psy_dir.position
                parent = psy_dir.parent
                # Remove the directive as we don't need it in the output now.
                psy_dir.detach()
                loops_to_skip.append(parent.children[position])
        for loop in routine.walk(Loop):
            if loop in loops_to_skip:
                continue
            OMPLoopTrans(omp_directive="paralleldo").apply(loop)
