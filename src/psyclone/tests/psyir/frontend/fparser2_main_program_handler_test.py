# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing pytest tests for the _main_program_handler method
in the class Fparser2Reader. This handler deals with the translation
of the fparser2 Main_Program construct to PSyIR.

'''
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
    correct = '''\
! PSyclone CodeBlock (unsupported code) reason:
!  - PSyclone doesn't yet support 'Contains' inside a Program
PROGRAM TestProgram
  CONTAINS
  SUBROUTINE TestSubroutine
  END SUBROUTINE
END PROGRAM
'''
    assert correct == out
