# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A PSyclone transformation script that transforms all synchronous
halo exchanges into asynchronous halo exchanges and moves the halo
exchange start part of each asynchronous halo exchange as early as
possible in the schedule in order to maximise the overlap of
communication and computation. Also outputs a textual view of the
transformed PSyIR representing the PSy-layer.

This is a generic implementation that will work for all LFRic
schedules and for algorithms containing multiple invoke calls.

This PSyclone transformation script is designed to be passed to
PSyclone, it is not designed to be run directly from python.

'''
from psyclone.psyir.nodes import Routine
from psyclone.transformations import LFRicAsyncHaloExchangeTrans
from psyclone.psyir.transformations import MoveTrans, TransformationError
from psyclone.lfric import LFRicHaloExchange, LFRicHaloExchangeStart


def trans(psyir):
    '''Transforms all synchronous halo exchanges into asynchronous halo
    exchanges and moves the halo exchange start part of each
    asynchronous halo exchange as early as possible in the schedule in
    order to maximise the overlap of communication and
    computation. Also outputs a textual view of the transformed PSyIR
    representing the PSy-layer.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    # Create the required transformations
    async_hex = LFRicAsyncHaloExchangeTrans()
    move_trans = MoveTrans()

    for subroutine in psyir.walk(Routine):
        # Split any synchronous halo exchanges into asynchronous halo exchanges
        for hex_node in subroutine.walk(LFRicHaloExchange):
            async_hex.apply(hex_node)

        # Move any halo exchange starts as early as possible in the
        # subroutine to maximise overlap of compute and comms within the
        # invoke.
        for hex_start_node in reversed(subroutine.walk(
                                                LFRicHaloExchangeStart)):
            idx = hex_start_node.position
            parent = hex_start_node.parent
            # Move halo exchange start node up one node at a time
            # until there is an exception (which indicates the move is
            # invalid). No need to check for idx == 0 as a negative
            # index wraps to the end of the list which will be
            # invalid.
            try:
                while True:
                    move_trans.apply(parent[idx], parent[idx-1])
                    idx -= 1
            except TransformationError:
                pass

    # Take a look at the modified PSy-layer PSyIR
    print(psyir.view())
