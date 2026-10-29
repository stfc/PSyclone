# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A PSyclone transformation script that outputs a textual representation
of the PSyIR representing the PSy-layer for the given algorithm layer code.

This PSyclone transformation script is designed to be passed to
PSyclone, it is not designed to be run directly from python.

'''


def trans(psyir):
    '''Output a textual view of the PSyIR representing the PSy-layer
    generated for the given algorithm layer code.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    # Take a look at the PSy-layer PSyIR
    print(psyir.view())
