# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. It adds kernel extraction code to
all invokes.
'''

from psyclone.domain.lfric.transformations import LFRicExtractTrans
from psyclone.psyir.nodes import Routine


def trans(psyir):
    '''
    Take the supplied PSyIR, and add kernel extraction code.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    extract = LFRicExtractTrans()

    for subroutine in psyir.walk(Routine):
        print(subroutine.name)

        # Apply the transformation
        extract.apply(subroutine, {"region_name": ("time_evolution",
                                                   subroutine.name),
                                   "create_driver": True})

        # Just as feedback: show the modified PSyIR, which should have
        # a new node at the top:
        print(subroutine.view())
