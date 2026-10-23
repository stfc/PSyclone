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

from psyclone.domain.lfric.transformations import (
    LFRicRedundantComputationTrans)
from psyclone.psyir.nodes import FileContainer
from psyclone.psyir.transformations import MoveTrans
from psyclone.transformations import LFRicAsyncHaloExchangeTrans


def trans(psyir: FileContainer) -> None:
    '''A sample transformation script to demonstrate the use of asynchronous
    halo exchanges with overlapping compute and communication for the
    most costly halo exchanges in the (current version of the) LFRic model.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    # Get first subroutine of the first module
    schedule = psyir.children[0].children[0]
    print(schedule.view())

    # This transformation removes the halo exchange associated with
    # the grad_p field. This transformation is unnecessary if
    # annexed_dofs is set to True in the config file (although the
    # transformation still works).
    rc_trans = LFRicRedundantComputationTrans()
    rc_trans.apply(schedule.children[0], {"depth": 1})
    print(schedule.view())

    # This transformation splits the three synchronous halo exchanges
    # (for fields p, hb_inv and u_normalisation) into asynchronous
    # (halo_exchange_start and halo_exchange_end) ones.
    ahex_trans = LFRicAsyncHaloExchangeTrans()
    for kern in schedule.children[3:0:-1]:
        ahex_trans.apply(kern)
    print(schedule.view())

    # This transformation moves the start of the three halo exchanges
    # before the setval_c loop offering the potential for overlap
    # between communication and computation.
    mtrans = MoveTrans()
    for kern in schedule.children[5:0:-2]:
        mtrans.apply(kern, schedule.children[0])
    print(schedule.view())
