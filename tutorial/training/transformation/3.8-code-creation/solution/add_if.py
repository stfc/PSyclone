# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
A generic transformation script that creates two different versions
of a loop, depending on iteration count.
'''

from typing import Optional

from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.transformations import OMPParallelLoopTrans
from psyclone.psyir.symbols import ScalarType
from psyclone.psyir.nodes import (BinaryOperation, FileContainer, IfBlock,
                                  Literal, Loop, Routine, Schedule)


def trans(psyir: FileContainer, parse_string: Optional[bool] = False) -> None:
    '''
    Create two versions of a loop, depending in iteration count.

    :param psyir: the PSyIR of the provided file.
    :param parse_string: whether the node code is created by parsing a
        Fortran string (True), or by assembling a PSyIR subtree (False).

    '''
    for routine in psyir.walk(Routine):
        if routine.name == "combine":
            break
    else:
        # Don't do anything if there is no combine subroutine
        return

    # combine_mod has only one outer loop, so take the first one:
    outer_loop = routine.walk(Loop)[0]

    # We start by creating the expression that will become the
    # if-condition. There are two ways of doing this:
    # 1. Parsing a Fortran expression given as string
    # 2. Creating the tree representation using the PSyIR API

    if parse_string:
        # Option 1: Create expression by parsing a Fortran string:
        writer = FortranWriter()
        expr_str = (f"{writer(outer_loop.stop_expr)} - "
                    f"{writer(outer_loop.start_expr)} >= 99")
        print(f"Parsing string '{expr_str}'.")
        reader = FortranReader()
        symbol_table = outer_loop.scope.symbol_table
        expr = reader.psyir_from_expression(expr_str,
                                            symbol_table)
    else:
        print("Creating tree")
        # Option 2: Create the PSyIR using the create methods:
        # `stop-start >= 99` as tree looks like this:
        # BinaryOperation ">="
        #     BinaryOperation "-"
        #         stop
        #         start
        #     "99"

        # Create `stop-start`:
        minus = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                       outer_loop.stop_expr.copy(),
                                       outer_loop.start_expr.copy())
        # Create `stop-start >= 99`:
        expr = BinaryOperation.create(BinaryOperation.Operator.GE,
                                      minus,
                                      Literal("99", ScalarType.integer_type()))

    # We first create a new if statement, with the above condition
    # and a copy of the loop as if-body, but no else body:
    copied_loop = outer_loop.copy()
    if_block = IfBlock.create(expr, [copied_loop])

    # Then we replace the original loop with this if statement
    # (which detaches the original loop):
    outer_loop.replace_with(if_block)

    # Then we add the original loop as child of the if_block
    # which means it becomes the else block:
    if_block.addchild(Schedule(children=[outer_loop]))

    # Now parallelise the copied loop (which is in the if body,
    # i.e. the case which has more than 100 iterations)
    ompt = OMPParallelLoopTrans()
    ompt.apply(copied_loop)
