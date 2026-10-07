# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Simple transformation script that prints out the names of the 'invoke'(s)
    in the supplied PSy-layer and the PSyIR for each. '''

from psyclone.psyGen import InvokeSchedule


def trans(psyir):
    '''
    This is an example that prints the names of the 'invoke'(s) and their
    associated PSyIR in the provided PSy-layer.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    names = [x.name for x in psyir.walk(InvokeSchedule)]
    print("Supplied code has Invokes: ", names)

    for schedule in psyir.walk(InvokeSchedule):
        print(schedule.view())
