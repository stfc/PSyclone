# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''File containing a PSyclone transformation script for the LFRic
API to apply loop fusion generically. Fusion is attempted for all
adjacent loops at the top level of a schedule. It will not fuse loops
that are lower in the schedule e.g. coloured loops. This can be
applied via the -s option in the psyclone script.

'''
from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans
from psyclone.transformations import TransformationError


def trans(psyir):
    '''PSyclone transformation script for the LFRic API to apply loop
    fusion generically to all top level loops.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    total_fused = 0
    lf_trans = LFRicLoopFuseTrans()

    for subroutine in psyir.children[0].children:
        # Loop over all nodes in reverse order
        idx = len(subroutine.children) - 1
        while idx > 0:
            node = subroutine.children[idx]
            prev_node = subroutine.children[idx-1]
            try:
                lf_trans.apply(prev_node, node, {"same_space": True})
                total_fused += 1
            except TransformationError:
                pass
            idx -= 1

    print(f"Fused {total_fused} loops")
