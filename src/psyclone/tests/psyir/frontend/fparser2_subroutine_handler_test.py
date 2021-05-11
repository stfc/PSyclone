# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing pytest tests for the _subroutine_handler method
in the class Fparser2Reader. This handler deals with the translation
of the fparser2 Subroutine_Subprogram construct to PSyIR.

'''
from __future__ import absolute_import
import pytest

from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.symbols import DataSymbol, ScalarType
from psyclone.psyir.nodes import Container, Routine
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.backend.fortran import FortranWriter

# subroutine no declarations
SUB1_IN = (
    "subroutine sub1()\n"
    "end subroutine\n")
SUB1_OUT = (
    "subroutine sub1()\n\n\n"
    "end subroutine sub1\n")
# subroutine with symbols/declarations
SUB2_IN = (
    "subroutine sub1(a)\n"
    "real :: a\n"
    "end subroutine\n")
SUB2_OUT = (
    "subroutine sub1(a)\n"
    "  real, intent(inout) :: a\n\n\n"
    "end subroutine sub1\n")
# subroutine with executable content
SUB3_IN = (
    "subroutine sub1()\n"
    "real :: a\n"
    "a=0.0\n"
    "end subroutine\n")
SUB3_OUT = (
    "subroutine sub1()\n"
    "  real :: a\n\n"
    "  a = 0.0\n\n"
    "end subroutine sub1\n")


@pytest.mark.parametrize("code,expected",
                         [(SUB1_IN, SUB1_OUT),
                          (SUB2_IN, SUB2_OUT),
                          (SUB3_IN, SUB3_OUT)])
def test_subroutine_handler(parser, code, expected):
    '''Test that subroutine_handler handles valid Fortran subroutines.'''

    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    parse_tree = parser(reader)
    subroutine = parse_tree.children[0]
    psyir = processor._subroutine_handler(subroutine, None)
    # Check the expected PSyIR nodes are being created
    assert isinstance(psyir, Routine)
    assert psyir.parent is None
    writer = FortranWriter()
    result = writer(psyir)
    assert expected == result


def test_function_handler(fortran_reader):
    '''Test that subroutine_handler correctly handles a function defined
    within a module.

    '''
    code = (
        "module a\n"
        "contains\n"
        "  function my_func()\n"
        "    integer :: my_func\n"
        "    my_func = 1\n"
        "  end function my_func\n"
        "end module\n")
    expected = (
        "module a\n"
        "  implicit none\n\n"
        "  public :: my_func\n\n"
        "  contains\n"
        "  function my_func()\n"
        "    integer :: my_func\n"
        "\n"
        "    my_func = 1\n"
        "\n"
        "  end function my_func\n"
        "\n"
        "end module a\n")
    psyir = fortran_reader.psyir_from_source(code)
    # Check PSyIR nodes are being created
    assert isinstance(psyir, Container)
    routines = psyir.walk(Routine)
    assert len(routines) == 1
    assert isinstance(routines[0].return_symbol, DataSymbol)
    assert (routines[0].return_symbol.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert psyir.parent is None
    writer = FortranWriter()
    result = writer(psyir)
    assert result == expected


def test_function_return_val(fortran_reader):
    '''
    Test that we handle a Fortran function with the return value specified
    using the 'result()' suffix.

    '''
    code = (
        "module a\n"
        "contains\n"
        "  function my_func() result(my_val)\n"
        "    real :: my_val\n"
        "    my_val = 1.0\n"
        "  end function my_func\n"
        "end module\n")
    psyir = fortran_reader.psyir_from_source(code)
    # Check PSyIR nodes are being created
    assert isinstance(psyir, Container)
    routines = psyir.walk(Routine)
    assert len(routines) == 1
    assert (routines[0].return_symbol is
            routines[0].symbol_table.lookup("my_val"))
    writer = FortranWriter()
    result = writer(psyir)
    assert ("  function my_func() result(my_val)\n"
            "    real :: my_val\n"
            "\n"
            "    my_val = 1.0\n"
            "\n"
            "  end function my_func\n" in result)
