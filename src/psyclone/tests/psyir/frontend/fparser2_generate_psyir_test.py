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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

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
