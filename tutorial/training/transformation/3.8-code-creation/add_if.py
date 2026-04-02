# flake8: noqa
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025-2026, Science and Technology Facilities Council
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author: J. Henrichs, Bureau of Meteorology

'''
A generic transformation script that creates two different versions
of a loop, depending on iteration count.
'''

import os


from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.transformations import OMPParallelLoopTrans
from psyclone.psyir.symbols import INTEGER_TYPE
from psyclone.psyir.nodes import (BinaryOperation, FileContainer, IfBlock,
                                  Literal, Loop, Routine, Schedule)


def trans(psyir: FileContainer) -> None:
    '''
    Create two versions of a loop, depending in iteration count.

    :param psyir: the PSyIR of the provided file.

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
    # Use an environment variable to select which one you
    # want to use

    if os.environ.get("PARSE_STRING", False):
        # Option 1: Create expression by parsing a Fortran string:
        writer = FortranWriter()
        # TODO: Create a string with the Fortran condition
        # loop_stop - loop_start >= 99
        expr_str = (f"{writer(TODO)} - "
                    f"{writer(TODO)} >= 99")
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
                                       TODO,
                                       TODO)
        # Create `stop-start >= 99`:
        expr = BinaryOperation.create(BinaryOperation.Operator.GE,
                                      TODO,
                                      Literal("99", INTEGER_TYPE))

    # We first create a new if statement, with the above condition
    # and a copy of the loop as if-body, but no else body:
    copied_loop = outer_loop.copy()
    if_block = IfBlock.create(expr, [copied_loop])

    # Then we replace the original loop with this if statement
    # (which detaches the original loop):
    TODO.replace_with(TODO)

    # Then add the original loop as child of the if_block
    # which means it becomes the else block:
    if_block.addchild(Schedule(children=[TODO]))

    # Now parallelise the copied loop (which is in the if body,
    # i.e. the case which has more than 100 iterations)
    ompt = OMPParallelLoopTrans()
    ompt.apply(copied_loop)
