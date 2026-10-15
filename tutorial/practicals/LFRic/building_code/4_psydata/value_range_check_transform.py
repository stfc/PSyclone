# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script passed to the psyclone command via the -s option. It
adds ValueRangeCheck code to the invokes.
'''


def trans(psyir):
    '''
    Add value_range_check verification code.

    :param psyir: the PSyIR of the generated PSy-layer
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''

    # ------------------------------------------------------
    # TODO: import the transformation and create an instance
    # ------------------------------------------------------
    # from ... import ...
    # my_transform = ...()

    for subroutine in psyir.children:

        # ------------------------------------------------------
        # TODO: Apply the transformation
        # ------------------------------------------------------
        ....apply(subroutine, {
                    "region_name": ("time_evolution", subroutine.name)})

        # Just as feedback: show the modified PSyIR, which should have
        # a new node at the top:
        print(subroutine.view())
