# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


'''PSyclone transformation script for the LFRic API to apply serial
optimisations: inline kernels into modules, expand intrinsics into code.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.psyir.nodes import IntrinsicCall, Routine, KernelSchedule
from psyclone.psyir.transformations import Matmul2CodeTrans
from psyclone.transformations import TransformationError


def trans(psyir):
    ''' Applies optimisations inside LFRic kernels.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''

    matmul_trans = Matmul2CodeTrans()
    inline_trans = KernelModuleInlineTrans()

    # Try to Inline all kernels into the PSy module
    for kernel in psyir.coded_kernels():
        try:
            inline_trans.apply(kernel)
            print(f"Module-inline transformation was successful for "
                  f"'{kernel.name}' in '{kernel.ancestor(Routine).name}'.")
        except TransformationError as err:
            print(f"Module-inline transformation failed for '{kernel.name}' "
                  f"in '{kernel.ancestor(Routine).name}' because:")
            print(str(err))

    # Then we transform all the kernels inlined into the module
    for kschedule in psyir.walk(KernelSchedule):
        # Expand MATMUL intrinsic
        for icall in kschedule.walk(IntrinsicCall):
            if icall.intrinsic == IntrinsicCall.Intrinsic.MATMUL:
                try:
                    matmul_trans.apply(icall)
                except TransformationError as err:
                    print(f"Inline MATMUL failed for '{kschedule.name}' "
                          f"because:")
                    print(str(err))
