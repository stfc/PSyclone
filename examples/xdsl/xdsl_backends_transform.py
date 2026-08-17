# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

from __future__ import print_function
import sys
from psyclone.psyir.nodes.routine import Routine
from backend.xdsl import xDSLWriter
from xdsl.printer import Printer
from xdsl.dialects.builtin import ModuleOp


def trans(psyir_root):

    '''
    :param psyir_root: root node of the IR.
    :type psyir_root: :py:class:`psyclone.psyir.nodes.FileContainer`
    '''

    writer = xDSLWriter()
    printer = Printer(stream=sys.stdout)

    routine_list = []
    for subroutine in psyir_root.walk(Routine):
        routine_list.append(writer(subroutine))

    top_level = ModuleOp(routine_list)
    printer.print_op(top_level)

    f = open("psy_output.mlir", "w")
    p2 = Printer(stream=f)
    p2.print_op(top_level)
    f.close()

    print("")
    print("")
    print("")
