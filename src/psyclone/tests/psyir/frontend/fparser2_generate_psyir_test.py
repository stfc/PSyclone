# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing pytest tests for the generate_psyir method in the
class Fparser2Reader. This method translates an fparser2 parse tree to
PSyIR.

'''
import pytest

from fparser.common.readfortran import FortranStringReader
from psyclone.errors import GenerationError
from psyclone.psyir.nodes import Container, Routine, FileContainer
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.backend.fortran import FortranWriter

MODULE_IN = (
    "module a\n"
    "contains\n"
    "subroutine sub1(a)\n"
    "real :: a\n"
    "end subroutine\n"
    "subroutine sub2\n"
    "end subroutine\n"
    "end module\n")
MODULE_OUT = (
    "module a\n"
    "  implicit none\n"
    "  public\n\n"
    "  contains\n"
    "  subroutine sub1(a)\n"
    "    real :: a\n\n\n"
    "  end subroutine sub1\n"
    "  subroutine sub2()\n\n\n"
    "  end subroutine sub2\n\n"
    "end module a\n")
SUB_IN = (
    "subroutine sub1()\n"
    "real :: a\n"
    "a=0.0\n"
    "end subroutine\n")
SUB_OUT = (
    "subroutine sub1()\n"
    "  real :: a\n\n"
    "  a = 0.0\n\n"
    "end subroutine sub1\n")
PROGRAM_IN = (
    "program main\n"
    "real :: a\n"
    "a=0.0\n"
    "end program main\n")
PROGRAM_OUT = (
    "program main\n"
    "  real :: a\n\n"
    "  a = 0.0\n\n"
    "end program main\n")
EMPTY_PROGRAM1_IN = (
    "program main\n"
    "end program main\n")
EMPTY_PROGRAM1_OUT = (
    "program main\n\n\n"
    "end program main\n")
EMPTY_PROGRAM2_IN = ""
EMPTY_PROGRAM2_OUT = ""
FUNCTION_IN = (
    "integer function tmp(a)\n"
    "real :: a\n"
    "a=0.0\n"
    "tmp = a\n"
    "end function tmp")
FUNCTION_OUT = (
    "function tmp(a)\n"
    "  real :: a\n"
    "  integer :: tmp\n\n"
    "  a = 0.0\n"
    "  tmp = a\n\n"
    "end function tmp\n")


@pytest.mark.parametrize("code,expected,node_class",
                         [(MODULE_IN, MODULE_OUT, Container),
                          (SUB_IN, SUB_OUT, Routine),
                          (PROGRAM_IN, PROGRAM_OUT, Routine),
                          (EMPTY_PROGRAM1_IN, EMPTY_PROGRAM1_OUT, Routine),
                          (EMPTY_PROGRAM2_IN, EMPTY_PROGRAM2_OUT, None),
                          (FUNCTION_IN, FUNCTION_OUT, Routine)])
def test_generate_psyir(parser, code, expected, node_class):
    '''Test that generate_psyir generates PSyIR from an fparser2 parse
    tree.

    '''
    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    parse_tree = parser(reader)
    psyir = processor.generate_psyir(parse_tree)
    # Check the expected PSyIR nodes are being created
    assert isinstance(psyir, FileContainer)
    if node_class:
        assert isinstance(psyir.children[0], node_class)
    writer = FortranWriter()
    result = writer(psyir)
    assert result == expected


def test_generate_psyir_error(parser):
    '''Test that generate_psyir raises the expected exception if the
    supplied fparser2 parse tree does not have a Program node as its
    root.'''
    processor = Fparser2Reader()
    reader = FortranStringReader(MODULE_IN)
    parse_tree = parser(reader)
    with pytest.raises(GenerationError) as info:
        _ = processor.generate_psyir(parse_tree.children[0])
    assert ("The Fparser2Reader generate_psyir method expects the root of "
            "the supplied fparser2 tree to be a Program, but found 'Module'"
            in str(info.value))
