# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Python script to visualise the differences between the DLS PSyIR tree, the
language-level PSyIR tree and the final output.
This script calls a successful exit from inside because it is a work in
progress of the development tracked by issue #1010.
'''

import sys
from psyclone.psyir.backend.fortran import FortranWriter


def trans(psyir):
    '''
    Prints to stdout the DLS PSyIR tree, the language-level PSyIR tree and the
    final Fortran code.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    schedule = next(x for x in psyir.children[0].children
                    if x.name == 'invoke_0_inc_field')

    print("DSL level view:")
    print(schedule.view())

    # In-place lowering to Language-level PSyIR
    schedule.lower_to_language_level()

    print("")
    print("Language level view:")
    print(schedule.view())

    fvisitor = FortranWriter()
    print("")
    print("FortranWriter code:")
    print(fvisitor(schedule.root))

    # This PSyclone call should terminate gracefully here
    sys.exit(0)
