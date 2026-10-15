# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module providing a PSyclone transformation script that converts the
Schedule of each Invoke to use OpenCL. '''

from psyclone.psyGen import InvokeSchedule
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.gocean.transformations import (
    GOOpenCLTrans, GOMoveIterationBoundariesInsideKernelTrans)
from psyclone.psyir.nodes import FileContainer
from psyclone.transformations import KernelImportsToArguments


def trans(psyir: FileContainer):
    '''
    Transformation routine for use with PSyclone. Converts any imported-
    variable accesses into kernel arguments and then applies the OpenCL
    transformation to the PSy layer.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    # Get the necessary transformations
    import_trans = KernelImportsToArguments()
    mod_inline_trans = KernelModuleInlineTrans()
    move_boundaries_trans = GOMoveIterationBoundariesInsideKernelTrans()
    cltrans = GOOpenCLTrans()

    for schedule in psyir.walk(InvokeSchedule):
        print("Converting to OpenCL invoke: " + schedule.name)

        # Skip invoke_2 as its time_smooth_code kernel contains a
        # module variable (alpha) which is not dealt with by the
        # KernelImportsToArguments transformation, see issue #826.
        if schedule.name == "invoke_2":
            continue

        # Remove the imports from inside each kernel and move PSy-layer
        # loop boundaries inside the kernel as a mask. To do this we must
        # first module-inline the kernel into the PSy layer module.
        for kern in schedule.kernels():
            print("Update kernel: " + kern.name)
            mod_inline_trans.apply(kern)
            move_boundaries_trans.apply(kern)
            import_trans.apply(kern)

        # Transform invoke to OpenCL
        cltrans.apply(schedule)
