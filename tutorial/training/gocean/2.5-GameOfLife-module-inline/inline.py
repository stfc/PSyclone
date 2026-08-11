# flake8: noqa
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone via the -s option.
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
    # TODO: Create an instance of the inline transformation

    km_inline = ...
    for invoke_sched in psyir.walk(InvokeSchedule):
        print("invoke", invoke_sched.name)
        for kern in invoke_sched.kernels():
            print("  kern", kern.name)

    # TODO: Look at the schedule representation, i.e. print
    # psyir.view()
    for kern in psyir.kernels():
        # TODO: Inline all kernels to help gfortran with inlining
        # by applying the inline transformation to each kernel:
        # inline.apply(...)
