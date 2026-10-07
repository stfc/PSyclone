# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module providing a transformation script that converts the Schedule of
    the first Invoke to use OpenCL. '''

from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.transformations import (
    FoldConditionalReturnExpressionsTrans)
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.gocean.transformations import (
    GOOpenCLTrans, GOMoveIterationBoundariesInsideKernelTrans)
from psyclone.psyir.nodes import FileContainer


def trans(psyir: FileContainer):
    '''
    Applies OpenCL to the given PSy-layer.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    mod_inline_trans = KernelModuleInlineTrans()
    ocl_trans = GOOpenCLTrans()
    fold_trans = FoldConditionalReturnExpressionsTrans()
    move_boundaries_trans = GOMoveIterationBoundariesInsideKernelTrans()

    # Provide kernel-specific OpenCL optimization options
    for idx, kern in enumerate(psyir.kernels()):
        # Kernel has to be module-inlined first.
        mod_inline_trans.apply(kern)
        # Move the PSy-layer loop boundaries inside the kernel as a kernel
        # mask, this allows to iterate through the whole domain
        move_boundaries_trans.apply(kern)
        # Change the syntax to remove the return statements introduced by the
        # previous transformation
        kschedules = kern.get_callees()
        # NOTE: we assume the kernel is not polymorphic and thus there is
        # only one schedule associated with it.
        fold_trans.apply(kschedules[0])
        # Specify the OpenCL queue and workgroup size of the kernel
        # In this case we dispatch each kernel in a different queue to check
        # that the output code has the necessary barriers to guarantee the
        # kernel execution order.
        kern.set_opencl_options({"queue_number": idx+1, 'local_size': 4})

    # Transform the Schedule
    for schedule in psyir.walk(InvokeSchedule):
        ocl_trans.apply(schedule, options={"end_barrier": True})
