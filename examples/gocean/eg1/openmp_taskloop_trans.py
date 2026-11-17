# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A simple test script showing the introduction of OpenMP tasking with
PSyclone.

'''

from psyclone.psyir.nodes import Loop
from psyclone.transformations import OMPSingleTrans
from psyclone.psyir.transformations import (
    OMPTaskloopTrans,
    OMPTaskwaitTrans,
    OMPParallelTrans
)


def trans(psyir):
    '''
    Transformation routine for use with PSyclone. Applies the OpenMP
    taskloop and taskwait transformations to the PSy layer.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    singletrans = OMPSingleTrans()
    paralleltrans = OMPParallelTrans()
    tasklooptrans = OMPTaskloopTrans(nogroup=False)
    taskwaittrans = OMPTaskwaitTrans()
    for schedule in psyir.children[0].children:
        print("Adding OpenMP tasking to invoke: " + schedule.name)
        for child in schedule.children:
            if isinstance(child, Loop):
                tasklooptrans.apply(child)
        singletrans.apply(schedule)
        paralleltrans.apply(schedule)
        taskwaittrans.apply(schedule[0])
