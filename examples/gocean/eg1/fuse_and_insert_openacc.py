# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' PSyclone script to fuse invoke_0 loops and insert OpenACC directives '''

from psyclone.psyir.nodes import FileContainer
from psyclone.psyGen import InvokeSchedule
from psyclone.psyGen import TransInfo
from psyclone.psyir.transformations import ACCLoopTrans, LoopFuseTrans


def trans(psyir: FileContainer):
    '''
    :param psyir: the PSyIR of the PSy-layer.

    '''
    invokes = psyir.walk(InvokeSchedule)

    # Print a list of all of the invokes found
    print([invoke.name for invoke in invokes])

    trans_info = TransInfo()
    print(trans_info.list)
    fuse_trans = LoopFuseTrans()
    ptrans = trans_info.get_trans_name('ACCParallelTrans')
    dtrans = trans_info.get_trans_name('ACCEnterDataTrans')
    ltrans = ACCLoopTrans()

    for invoke in invokes:
        if invoke.name == "invoke_0":
            # fuse all outer loops
            fuse_trans.apply(invoke.children[0], invoke.children[1])
            fuse_trans.apply(invoke.children[0], invoke.children[1])
            fuse_trans.apply(invoke.children[0], invoke.children[1])
            # fuse all inner loops
            fuse_trans.apply(invoke.children[0].loop_body[0],
                             invoke.children[0].loop_body[1])
            fuse_trans.apply(invoke.children[0].loop_body[0],
                             invoke.children[0].loop_body[1])
            fuse_trans.apply(invoke.children[0].loop_body[0],
                             invoke.children[0].loop_body[1])
            # Apply an OpenACC loop directive to the loop
            ltrans.apply(invoke.children[0], {"collapse": 2})

            # Create an OpenACC parallel region around the loop
            ptrans.apply(invoke.children[0])

            # Add an OpenACC enter-data directive
            dtrans.apply(invoke)
