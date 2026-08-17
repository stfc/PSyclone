# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''File containing a PSyclone transformation script for the LFRic
API to apply OpenACC Kernels and Enter Data directives generically. Any
user-supplied kernels are also transformed through the addition of an OpenACC
Routine directive. PSyclone can apply this transformation script via its
 -s option.

'''
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.lfric import LFRicConstants
from psyclone.psyGen import CodedKern, InvokeSchedule
from psyclone.psyir.transformations import ACCKernelsTrans
from psyclone.transformations import (
    ACCEnterDataTrans, ACCRoutineTrans, LFRicColourTrans)


def trans(psyir):
    '''PSyclone transformation script for the LFRic API to apply OpenACC
    kernels and enter data directives generically. User-supplied kernels are
    transformed through the addition of a routine directive.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    const = LFRicConstants()

    ctrans = LFRicColourTrans()
    enter_data_trans = ACCEnterDataTrans()
    mod_inline_trans = KernelModuleInlineTrans()
    kernel_trans = ACCKernelsTrans()
    rtrans = ACCRoutineTrans()

    # Loop over all of the Invokes Schedules
    for subroutine in psyir.walk(InvokeSchedule):

        print(f"Transforming invoke '{subroutine.name}'...")

        # Colour loops over cells unless they are on discontinuous
        # spaces or over dofs
        for loop in subroutine.loops():
            if loop.iteration_space.endswith("cell_column"):
                if (loop.field_space.orig_name not in
                        const.VALID_DISCONTINUOUS_NAMES):
                    ctrans.apply(loop)

        for loop in subroutine.loops():
            if loop.loop_type not in ["colours", "null"]:
                kernel_trans.apply(loop)

        enter_data_trans.apply(subroutine)

        # We transform every user-supplied kernel using ACCRoutineTrans. This
        # adds '!$acc routine' which ensures the kernel is compiled for the
        # OpenACC device.
        for kernel in subroutine.walk(CodedKern):
            # Module inlining is a pre-requisite for kernel transformations.
            mod_inline_trans.apply(kernel)
            rtrans.apply(kernel)
