# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''File containing a PSyclone transformation script for the LFRic
API to apply OpenACC Loop, Parallel and Enter Data directives
generically. This can be applied via the -s option in the psyclone
command, it is not designed to be directly run from python.

'''
from psyclone.psyir.nodes import Routine
from psyclone.psyir.transformations import ACCKernelsTrans
from psyclone.transformations import (
    ACCEnterDataTrans, ACCLoopTrans, ACCRoutineTrans, LFRicColourTrans)
from psyclone.domain.lfric import LFRicConstants


def trans(psyir):
    '''PSyclone transformation script for the LFRic api to apply
    OpenACC loop, parallel and enter data directives generically.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    kernels_trans = ACCKernelsTrans()
    routine_trans = ACCRoutineTrans()
    ctrans = LFRicColourTrans()
    loop_trans = ACCLoopTrans()
    enter_trans = ACCEnterDataTrans()
    const = LFRicConstants()

    for subroutine in psyir.walk(Routine):

        # Colour loops as required
        for loop in subroutine.loops():
            if loop.field_space.orig_name \
               not in const.VALID_DISCONTINUOUS_NAMES \
               and loop.iteration_space == "cell_column":
                ctrans.apply(loop)

        # Add Kernels and Loop directives
        for loop in subroutine.loops():
            if loop.loop_type != "colours":
                kernels_trans.apply([loop])
                loop_trans.apply(loop)

        # Add Routine directive to kernels
        for kernel in subroutine.coded_kernels():
            routine_trans.apply(kernel)

        # Add Enter Data directive covering all of the PSy layer.
        enter_trans.apply(subroutine)

        print(subroutine.view())
