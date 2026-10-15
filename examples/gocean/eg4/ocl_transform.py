# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module providing a transformation script that converts the Schedule of
    the first Invoke to use OpenCL. In order to do this, those kernels that
    access imported data are transformed so as to pass that data by argument.
'''

from psyclone.transformations import KernelImportsToArguments
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.gocean.transformations import (
    GOOpenCLTrans, GOMoveIterationBoundariesInsideKernelTrans)
from psyclone.psyir.nodes import FileContainer


def trans(psyir: FileContainer):
    '''
    Transformation routine for use with PSyclone. Applies the OpenCL
    transform to the first Invoke in the PSy-layer.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    # Convert any kernel accesses to imported data into arguments
    mod_inline_trans = KernelModuleInlineTrans()
    ktrans = KernelImportsToArguments()
    for kern in psyir.kernels():
        mod_inline_trans.apply(kern)
        ktrans.apply(kern)

    # Provide kernel-specific OpenCL optimization options
    move_boundaries_trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kern in psyir.kernels():
        # Move the PSy-layer loop boundaries inside the kernel as a kernel
        # mask, this allows to iterate through the whole domain
        move_boundaries_trans.apply(kern)
        # Specify the OpenCL queue and workgroup size of the kernel
        kern.set_opencl_options({"queue_number": 1, 'local_size': 4})

    # Transform the Schedule of the first invoke
    cltrans = GOOpenCLTrans()
    cltrans.apply(psyir.children[0].children[0], options={"end_barrier": True})
