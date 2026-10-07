# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. Transforms all kernels in the invoke
to have them compiled for an OpenACC accelerator. '''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.transformations import (
    ACCParallelTrans, ACCEnterDataTrans, ACCRoutineTrans,
    KernelImportsToArguments)
from psyclone.psyir.nodes import Loop
from psyclone.psyir.transformations import ACCLoopTrans


def trans(psyir):
    ''' Take the supplied psy-layer, apply OpenACC transformations
    to the schedule of the first invoke.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    ptrans = ACCParallelTrans()
    ltrans = ACCLoopTrans()
    dtrans = ACCEnterDataTrans()
    ktrans = ACCRoutineTrans()
    mod_inline_trans = KernelModuleInlineTrans()
    g2localtrans = KernelImportsToArguments()

    schedule = psyir.children[0].children[0]

    # Apply the OpenACC Loop transformation to *every* loop
    # nest in the schedule
    for child in schedule.children:
        if isinstance(child, Loop):
            ltrans.apply(child, {"collapse": 2})

    # Put all of the loops in a single parallel region
    ptrans.apply(schedule.children)

    # Add an enter-data directive
    dtrans.apply(schedule)

    # Convert any accesses to imported data into kernel arguments, put an
    # 'acc routine' directive inside, and module-inline each kernel
    for kern in schedule.coded_kernels():
        mod_inline_trans.apply(kern)
        if kern.name == "kern_use_var_code":
            g2localtrans.apply(kern)
        ktrans.apply(kern)
