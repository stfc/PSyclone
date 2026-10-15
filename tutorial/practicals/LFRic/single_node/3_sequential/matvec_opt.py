# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


'''An example PSyclone transformation script to demonstrate
optimisations to the matrix vector kernel to improve its performance
on CPUs. It replaces the matmul Fortran intrinsic with inline matrix
vector code.

This script can be applied via the -s option to the psyclone
command, it is not designed to be directly run from python.

'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.psyir.nodes import IntrinsicCall
from psyclone.psyir.transformations import Matmul2CodeTrans


def trans(psyir):
    '''PSyclone transformation script for the LFRic API to optimise
    the matvec kernel for many-core CPUs. This is currently limited to
    running on the scaled_matrix_vector_code kernel but should work
    more generally. Any matmul calls are replaced with inline matrix
    vector code.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    matmul2code_trans = Matmul2CodeTrans()
    mod_inline_trans = KernelModuleInlineTrans()

    for kernel in psyir.coded_kernels():
        if kernel.name.lower() == "scaled_matrix_vector_code":
            mod_inline_trans.apply(kernel)
            for kernel_schedule in kernel.get_callees():
                # Replace matmul with inline code
                for icall in kernel_schedule.walk(IntrinsicCall):
                    if icall.intrinsic == IntrinsicCall.Intrinsic.MATMUL:
                        matmul2code_trans.apply(icall)
                print(kernel_schedule.view())
