# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. It adds read-only verification code to
the invokes.
'''
from psyclone.psyir.nodes import Routine


def trans(psyir):
    '''
    Add read-only verification code.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''

    # ------------------------------------------------------
    # TODO: import the transformation and create an instance
    # ------------------------------------------------------
    # from ... import ...
    # my_transform = ...()

    for subroutine in psyir.walk(Routine):
        print(subroutine.name)

        # ------------------------------------------------------
        # TODO: Apply the transformation
        # ------------------------------------------------------
        ....apply(subroutine, {"region_name": ("time_evolution",
                                               subroutine.name)})

        # Just as feedback: show the modified PSyIR, which should have
        # a new node at the top:
        print(subroutine.view())
