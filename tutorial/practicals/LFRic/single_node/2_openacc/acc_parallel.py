# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''File containing a PSyclone transformation script for the LFRic
API to apply the Kernels directive to all loops generically. This can
be applied via the -s option in the psyclone command, it is not
designed to be directly run from python.

'''
from psyclone.psyir.transformations import ACCKernelsTrans
from psyclone.transformations import (
    ACCEnterDataTrans, ACCLoopTrans, ACCRoutineTrans, LFRicColourTrans)
from psyclone.domain.lfric.function_space import FunctionSpace


def trans(psyir):
    '''PSyclone transformation script for the LFRic api to apply
    OpenACC Kernels directives to all loops generically. It also
    outputs a textual representation of the transformed PSyIR.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`


    '''
    kernels_trans = ACCKernelsTrans()

    # Apply kernels directives to any loop nodes that are
    # children of the schedule node.
    for loop in psyir.loops():
        kernels_trans.apply([loop])

    print(psyir.view())
