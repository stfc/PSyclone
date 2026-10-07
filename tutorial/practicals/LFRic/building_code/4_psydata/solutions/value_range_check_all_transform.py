# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script passed to the psyclone command via the -s option. It
adds ValueRangeCheck code to the invokes.
'''

from psyclone.psyir.nodes import Routine
from psyclone.psyir.transformations import ValueRangeCheckTrans


def trans(psyir):
    '''
    Add value_range_check verification code.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    value_range_check = ValueRangeCheckTrans()

    for subroutine in psyir.walk(Routine):
        print(subroutine.name)

        # Apply the transformation
        value_range_check.apply(subroutine, {"region_name":
                                             ("time_evolution",
                                              subroutine.name)})

        # Just as feedback: show the modified PSyIR, which should have
        # a new node at the top:
        print(subroutine.view())
