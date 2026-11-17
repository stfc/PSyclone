# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone's generate()
function via the -s option.
This script calls a successful exit from inside because it is a work in
progress of the development tracked by issue #2905.
'''

import sys
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.backend.fortran import FortranWriter


def trans(psyir):
    ''' Use the PSyIR back-end to generate PSy-layer target code.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    # Loop over all of the Invokes Schedules
    for schedule in psyir.walk(InvokeSchedule):

        print(f"Transforming invoke '{schedule.name}'...")

        print("DSL level view:")
        print(schedule.view())

    # TODO #2905: This script should terminate here until LFRic declares
    # all its symbols to the symbol table.
    sys.exit(0)

    for schedule in psyir.walk(InvokeSchedule):
        # In-place lowering to Language-level PSyIR
        print(schedule.symbol_table.view())
        schedule.lower_to_language_level()

        print("")
        print("Language level view:")
        print(schedule.view())

    print("")
    print("FortranWriter code:")
    fvisitor = FortranWriter()
    print(fvisitor(psyir))
