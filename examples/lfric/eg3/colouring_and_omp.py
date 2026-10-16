# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


''' File containing a PSyclone transformation script for the LFRic
API to apply colouring and OpenMP generically. This can be applied via
the -s option in the "psyclone" script. '''
from psyclone.domain.lfric.transformations import LFRicColourAndOMPTrans
from psyclone.psyir.nodes import Routine


def trans(psyir):
    ''' PSyclone transformation script for the LFRic API to apply
    colouring and OpenMP generically.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    ctrans = LFRicColourAndOMPTrans()

    # Loop over all of the Invokes subroutines
    for subroutine in psyir.walk(Routine):

        print(f"Transforming invoke '{subroutine.name}' ...")
        ctrans.apply(subroutine)
