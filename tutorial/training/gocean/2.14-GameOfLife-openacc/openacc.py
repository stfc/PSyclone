# flake8: noqa
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone via the -s option.
It adds OpenACC directives to execute the code on GPUs.
'''

from psyclone.transformations import (ACCParallelTrans, ACCEnterDataTrans,
                                      ACCLoopTrans, ACCRoutineTrans)
from psyclone.psyir.nodes import FileContainer, Loop
from psyclone.gocean1p0 import GOKern
from psyclone.psyGen import InvokeSchedule

from fuse_loops import trans as fuse_trans


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object, use the existing fuse_loops
    script to do module inlining and fuse the first three loops,
    then apply OpenACC directives. 

    :param psyir: the PSyIR layer to transform.


    '''
    # Use existing fuse script to fuse all loops
    fuse_trans(psyir)

    # Module inline all kernels (so they can be modified)
    # Then add an acc routine statement to each of them:
    ktrans = ACCRoutineTrans()
    for kern in psyir.walk(GOKern):
        # Put an 'acc routine' directive inside each kernel
        ktrans.apply(kern)

    # Now parallelise each schedule with openacc:
    ptrans = ACCParallelTrans()
    ltrans = ACCLoopTrans()
    dtrans = ACCEnterDataTrans()
    for schedule in psyir.walk(InvokeSchedule):
        # Apply the OpenACC Loop transformation to *every* loop
        # nest in the schedule (which are all outer loops).
        for child in schedule.children:
            TODO1: apply ltrans
            TODO1:  bonus points: when using MPI, there could be
            TODO1:  non-loop statement in the schedule (halo exchange)
            TODO1:  So only apply this if the child is a loop


        # Put all of the loops in a single parallel region
        TODO2: apply ptrans around the whole schedule

        # Add an enter-data directive
        TODO3: apply dtrans to the schedule
