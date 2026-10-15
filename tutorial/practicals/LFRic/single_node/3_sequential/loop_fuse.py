# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''File containing a PSyclone transformation script for the LFRic
API to apply loop fusion.

This script can be applied via the -s option to the psyclone command,
it is not designed to be directly run from python.

'''
from psyclone.transformations import LFRicLoopFuseTrans, TransformationError


def trans(psyir):
    '''PSyclone transformation script for the LFRic API to apply loop
    fusion for a particular example - it is not meant to work
    generically.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    ftrans = LFRicLoopFuseTrans()

    for subroutine in psyir.children[0].children:

        try:
            while True:
                ftrans.apply(subroutine[0], subroutine[1])
        except TransformationError as info:
            print(str(info.value))

        # take a look at what we've done
        print(subroutine.view())
