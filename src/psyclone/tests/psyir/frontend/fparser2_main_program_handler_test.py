# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Author R. W. Ford, A. B. G. Chalk, STFC Daresbury Lab

'''Module containing pytest tests for the _main_program_handler method
in the class Fparser2Reader. This handler deals with the translation
of the fparser2 Main_Program construct to PSyIR.

'''
from __future__ import absolute_import
import pytest

from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.nodes import CodeBlock, Routine
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.backend.fortran import FortranWriter

# program no declarations
PROG1_IN = (
    "program prog\n"
    "end program prog\n")
PROG1_OUT = (
    "program prog\n\n\n"
    "end program prog\n")
# program with symbols/declarations
PROG2_IN = (
    "program prog\n"
    "real :: a\n"
    "end program\n")
PROG2_OUT = (
    "program prog\n"
    "  real :: a\n\n\n"
    "end program prog\n")
# program with executable content
PROG3_IN = (
    "program prog\n"
    "real :: a\n"
    "a=0.0\n"
    "end\n")
PROG3_OUT = (
    "program prog\n"
    "  real :: a\n\n"
    "  a = 0.0\n\n"
    "end program prog\n")


@pytest.mark.parametrize("code,expected",
                         [(PROG1_IN, PROG1_OUT),
                          (PROG2_IN, PROG2_OUT),
                          (PROG3_IN, PROG3_OUT)])
def test_main_program_handler(parser, code, expected):
    '''Test that main_program_handler handles valid Fortran subroutines.'''

    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    parse_tree = parser(reader)
    program = parse_tree.children[0]
    psyir = processor._main_program_handler(program, None)
    # Check the expected PSyIR nodes are being created
    assert isinstance(psyir, Routine)
    assert psyir.is_program
    assert psyir.parent is None
    writer = FortranWriter()
    result = writer(psyir)
    assert expected == result


def test_main_program_handler_codeblock(fortran_reader, fortran_writer):
    '''Test the main_program_handler results in a CodeBlock if the input
    code contains a child Subroutine.'''
    code = '''Program TestProgram

    contains
    Subroutine TestSubroutine()
    End Subroutine
    End Program'''

    psyir = fortran_reader.psyir_from_source(code)
    cblock = psyir.children[0]
    assert isinstance(cblock, CodeBlock)
    out = fortran_writer(psyir)
    correct = '''PROGRAM TestProgram
  CONTAINS
  SUBROUTINE TestSubroutine
  END SUBROUTINE
END PROGRAM
'''
    assert correct == out
