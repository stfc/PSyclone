# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A PSyclone transformation script that transforms a specific
synchronous halo exchange into an asynchronous halo exchange and
moves the halo exchange start part as early as possible in the
schedule in order to maximise the overlap of communication and
computation. Also outputs a textual view of the transformed PSyIR
representing the PSy-layer.

This is a kernel-specific implementation that will only work for
schedules with a halo exchange in a hard-coded location and a
resultant halo exchange start that can be moved to a hard-coded
location.

It is designed to work with the helmholtz example with the annexed
dofs option set to false.

This PSyclone transformation script is designed to be passed to
PSyclone, it is not designed to be run directly from python.

'''
from psyclone.domain.lfric import LFRicLoop
from psyclone.lfric import LFRicHaloExchange, LFRicHaloExchangeStart
from psyclone.transformations import LFRicAsyncHaloExchangeTrans
from psyclone.psyir.transformations import MoveTrans


def trans(psyir):
    '''Transforms a specific synchronous halo exchange into an
    asynchronous halo exchange and moves the halo exchange start part
    as early as possible in the schedule in order to maximise the
    overlap of communication and computation. Also outputs a textual
    view of the transformed PSyIR representing the PSy-layer.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    # Create the required transformations
    async_hex_trans = LFRicAsyncHaloExchangeTrans()
    move_trans = MoveTrans()

    # Get the first subroutine in the first module
    schedule = psyir.children[0].children[0]
    # Find the halo exchange for div_star (the third exchange in the
    # original schedule).
    hex_node = next(node for node in schedule.walk(LFRicHaloExchange)
                    if node.field.name == "div_star")
    async_hex_trans.apply(hex_node)

    # Move the (specific) halo exchange start node to the start of the
    # schedule
    hex_start = next(node for node in schedule.walk(LFRicHaloExchangeStart)
                     if node.field.name == "div_star")
    first_loop = schedule.walk(LFRicLoop)[0]
    move_trans.apply(hex_start, first_loop)

    # Take a look at the modified PSy-layer PSyIR
    print(schedule.view())
