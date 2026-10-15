# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. It adds kernel NAN-verification to
the invokes. This then creates code that, at runtime, verifies that
all input and output parameters of a region are a valid number, i.e.
not infinity or NAN.
'''

from psyclone.psyir.nodes import Loop
from psyclone.psyir.transformations import ValueRangeCheckTrans


def trans(psyir):
    '''
    Add verification to both invokes that read only parameters are
    not modified.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''

    value_range_check = ValueRangeCheckTrans()

    for loop in psyir.walk(Loop):
        if loop.ancestor(Loop):
            # Only instrument outer loops. So if this loop is inside
            # another loop, don't do anything.
            continue
        value_range_check.apply(loop)
        # You can specify a module and region name adding
        # options={"region_name": ("main", "init")})
