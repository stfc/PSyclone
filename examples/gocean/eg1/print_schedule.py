# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' PSyclone script that print the invoke names and their schedules '''

from psyclone.psyir.nodes import FileContainer
from psyclone.psyGen import InvokeSchedule


def trans(psyir: FileContainer):
    '''
    :param psyir: the PSyIR of the PSy-layer.

    '''
    invokes = psyir.walk(InvokeSchedule)

    # Print a list of all of the invokes found
    print([invoke.name for invoke in invokes])

    # Print the Schedule of each of these Invokes
    for invoke in invokes:
        print(invoke.view())
