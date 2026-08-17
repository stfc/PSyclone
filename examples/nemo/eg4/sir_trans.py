# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module providing a transformation script that converts the supplied
PSyIR to the Stencil intermediate representation (SIR).
'''

from psyclone.psyir.backend.sir import SIRWriter
from psyclone.psyir.nodes import Routine


def trans(psyir):
    '''Transformation routine for use with PSyclone. Applies the PSyIR2SIR
    transform to the routines in the supplied PSyIR.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    sir_writer = SIRWriter()
    for subroutine in psyir.walk(Routine):
        print(f"Transforming subroutine: {subroutine.name}")
        try:
            sir_code = sir_writer(subroutine)
            print(sir_code)
        except Exception as e:
            print(f"Failed to transform {subroutine.name}: {e}")
