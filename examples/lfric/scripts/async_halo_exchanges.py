# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''File containing a PSyclone transformation script for the LFRic
API to make asynchronous halo exchanges and overlap their
communication with computation. This can be applied via the -s option
in the generator.py script.

'''

from psyclone.lfric import LFRicHaloExchange, LFRicHaloExchangeStart
from psyclone.psyGen import InvokeSchedule
from psyclone.transformations import LFRicAsyncHaloExchangeTrans
from psyclone.psyir.transformations import MoveTrans, TransformationError


def trans(psyir):
    '''A transformation script to use asynchronous halo exchanges with
    overlapping compute and communication for the LFRic model.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    for subroutine in psyir.walk(InvokeSchedule):
        # This transformation splits the three synchronous halo exchanges
        ahex_trans = LFRicAsyncHaloExchangeTrans()
        for h_ex in subroutine.walk(LFRicHaloExchange):
            ahex_trans.apply(h_ex)

        # This transformation moves the start of the halo exchanges as far
        # as possible offering the potential for overlap between communication
        # and computation.
        mtrans = MoveTrans()
        location_cursor = 0
        for ahex in subroutine.walk(LFRicHaloExchangeStart):
            if ahex.position <= location_cursor:
                continue
            try:
                mtrans.apply(ahex, subroutine.children[location_cursor])
                location_cursor += 1
            except TransformationError:
                pass

        print(f"{location_cursor} AsyncHaloExchanges have been rearranged"
              f" in {subroutine.name}")
