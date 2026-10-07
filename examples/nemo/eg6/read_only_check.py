# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A transformation script that applies read-only-verification
to a small Fortran program. You can use
    $ psyclone --config ../../../psyclone.cfg  \
        -s ./read_only_check.py -opsy psy.f90 dummy.f90

'''

from psyclone.psyir.transformations import ReadOnlyVerifyTrans
from psyclone.psyir.nodes import Loop, Routine


def trans(psyir):
    '''Applies the read-only verification transformation to every
    subroutine in the file.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`
    '''

    rov = ReadOnlyVerifyTrans()

    for subroutine in psyir.walk(Routine):
        print(f"Transforming subroutine: {subroutine.name}")
        for kern in subroutine.children:
            if not isinstance(kern, Loop):
                continue
            rov.apply(kern)
