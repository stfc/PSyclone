# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. It adds kernel read-only-verification to
the invokes. This then creates code that, at runtime, verifies that
all read-only entities passed to the kernel have not been modified.
'''

from psyclone.psyir.transformations import ReadOnlyVerifyTrans
from psyclone.psyGen import InvokeSchedule


def trans(psyir):
    '''
    Add verification checks to both invokes that read only parameters are not
    modified.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    read_only_verify = ReadOnlyVerifyTrans()

    for schedule in psyir.walk(InvokeSchedule):
        if schedule.name == "invoke_0":
            # You could just apply the transform for all subroutines, but
            # in this case we also want to give the regions a friendlier name:
            read_only_verify.apply(schedule.children,
                                   {"region_name": ("main", "init")})

        if schedule.name == "invoke_1_update_field":
            # Enclose everything in a read_only_verify region
            read_only_verify.apply(schedule.children,
                                   {"region_name": ("main", "update")})
