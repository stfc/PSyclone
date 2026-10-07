# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A PSyclone transformation script which performs redundant
computation to remove halo exchanges where possible and then moves the
remaining ones to the beginning of the loop thereby separating the
computation from the communication. The science code for which this
example script has been written is taken from the Met Office
repository but an operator has been replaced with a field in one of
the kernels to allow redundant computation'''

from psyclone.domain.lfric.transformations import (
    LFRicRedundantComputationTrans)
from psyclone.psyir.nodes import FileContainer
from psyclone.psyir.transformations import MoveTrans


def trans(psyir: FileContainer) -> None:
    '''Removes the grad_p halo exchanges by redundant computation then
    moves the remaining halo exchanges to the beginning of the invoke
    call.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    rc_trans = LFRicRedundantComputationTrans()
    m_trans = MoveTrans()

    # Get first invoke subroutine
    schedule = psyir.children[0].children[0]

    # redundant computation to remove grad_p halo exchanges
    rc_trans.apply(schedule.children[5], {"depth": 2})
    rc_trans.apply(schedule.children[0], {"depth": 2})

    # move remaining (potential) halo exchanges to start of the invoke
    m_trans.apply(schedule.children[0], schedule.children[4])
