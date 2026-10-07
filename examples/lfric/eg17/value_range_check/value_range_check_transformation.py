# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone via the -s option.
It adds kernel extraction code to
the invokes. When the transformed program is compiled and run, it
will create one NetCDF file for each of the two invokes. A separate
driver program is also created for each invoke which can read the
created NetCDF files, execute the invokes and then compare the results.
'''

from psyclone.psyir.nodes import Routine
from psyclone.psyir.transformations import ValueRangeCheckTrans


def trans(psyir):
    '''
    Add kernel extraction code.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    vrc = ValueRangeCheckTrans()

    for subroutine in psyir.walk(Routine):
        vrc.apply(subroutine)
