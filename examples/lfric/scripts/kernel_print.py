# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''PSyclone script demonstrating that kernels that have been
transformed into the PSyIR can be transformed back into Fortran by
using the FortranWriter class.

'''
from psyclone.psyir.backend.fortran import FortranWriter


def trans(psyir):
    '''Print out Fortran versions of all kernels found in this file.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    fortran_writer = FortranWriter()

    already_printed = []

    # Loop over all of the Kernels Calls
    for kernel in psyir.coded_kernels():
        try:
            for ksched in kernel.get_callees():
                if ksched not in already_printed:
                    kern = fortran_writer(ksched)
                    print(kern)
                already_printed.append(ksched)
        except Exception as err:  # pylint: disable=broad-except
            print(f"Code of '{kernel.name}' "
                  f"cannot be printed because:\n{err}")
