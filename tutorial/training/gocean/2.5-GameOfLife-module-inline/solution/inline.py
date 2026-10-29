# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone' via the -s option.
It module inlines all kernels.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object, and module inline all kernels

    :param psyir: the PSyIR of the PSy-layer.

    '''
    km_inline = KernelModuleInlineTrans()

    for invoke_sched in psyir.walk(InvokeSchedule):
        print("invoke", invoke_sched.name)
        for kern in invoke_sched.kernels():
            print("  kern", kern.name)

    # Or to show that InvokesSchedule are Routines:
    # from psyclone.psyir.nodes import Routine
    # for subroutine in psyir.walk(Routine):
    #    print(subroutine.view())

    for kern in psyir.kernels():
        # Inline all kernels to help gfortran with inlining.
        km_inline.apply(kern)
