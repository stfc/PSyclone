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

from psyclone.psyir.transformations import ValueRangeCheckTrans


def trans(psyir):
    '''
    Add verification to both invokes that read only parameters are
    not modified.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    value_range_check = ValueRangeCheckTrans()

    for schedule in psyir.children[0].children:
        if schedule.name == "invoke_0":
            # You could just apply the transform for all subroutines, but
            # in this case we also want to give the regions a friendlier name:
            value_range_check.apply(schedule.children,
                                    {"region_name": ("main", "init")})

        if schedule.name == "invoke_1_update_field":
            # Enclose everything in a value_range_check region
            value_range_check.apply(schedule.children,
                                    {"region_name": ("main", "update")})
