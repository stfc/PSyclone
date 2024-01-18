# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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

'''Performs pytest tests on the parsing of Part_Ref in the fparser2
   PSyIR front-end.

'''
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Execution_Part

from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import KernelSchedule, Routine, Call, ArrayReference
from psyclone.psyir.symbols import SymbolError, DataSymbol, ScalarType, \
    INTEGER_TYPE, RoutineSymbol, ArrayType


@pytest.mark.usefixtures("f2008_parser")
def test_handling_part_ref():
    '''Test that fparser2 Part_Ref is converted to the expected PSyIR
    tree structure.

    '''
    reader = FortranStringReader("x(2)=1")
    fparser2part_ref = Execution_Part.match(reader)[0][0]

    fake_parent = KernelSchedule('kernel')
    fake_parent.symbol_table.add(DataSymbol('x', INTEGER_TYPE))

    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2part_ref])

    assert len(fake_parent.children) == 1
    assignment = fake_parent.children[0]
    assert len(assignment.children) == 2
    new_node = assignment.children[0]
    assert isinstance(new_node, ArrayReference)
    assert new_node.name == "x"
    assert len(new_node.children) == 1  # Array dimensions


@pytest.mark.usefixtures("f2008_parser")
def test_handling_part_ref_expression():
    '''Test that fparser2 Part_Ref is converted to the expected PSyIR
    tree structure when there is a complex expression.

    '''
    # Parse a complex array expression
    reader = FortranStringReader("x(i+3,j-4,(z*5)+1)=1")
    fparser2part_ref = Execution_Part.match(reader)[0][0]

    fake_parent = KernelSchedule('assign')
    array_type = ArrayType(INTEGER_TYPE, [10, 10, 10])
    fake_parent.symbol_table.add(DataSymbol('x', array_type))
    fake_parent.symbol_table.add(DataSymbol('i', INTEGER_TYPE))
    fake_parent.symbol_table.add(DataSymbol('j', INTEGER_TYPE))
    fake_parent.symbol_table.add(DataSymbol('z', INTEGER_TYPE))

    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2part_ref])
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent[0].lhs
    assert isinstance(new_node, ArrayReference)
    assert new_node.name == "x"
    assert len(new_node.children) == 3  # Array dimensions


def test_handling_part_ref_function(fortran_reader):
    '''Test that fparser2 Part_Ref is converted to the expected PSyIR
    tree structure when there is a function. The function will be a
    part_ref but will have a RoutineSymbol.

    '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine test_sub()\n"
        "    integer :: a\n"
        "    integer :: i\n"
        "    a = test_func(i)\n"
        "  end subroutine\n"
        "  integer function test_func(i)\n"
        "    integer :: i\n"
        "    test_func = i\n"
        "  end function test_func\n"
        "end module\n")
    psyir = fortran_reader.psyir_from_source(code)

    function = psyir.children[0].children[1]
    assert isinstance(function, Routine)
    assert isinstance(function.return_symbol, DataSymbol)
    assert isinstance(function.return_symbol.datatype, ScalarType)
    call = psyir.children[0].children[0].children[0].rhs
    assert isinstance(call, Call)
    assert call.routine.name == "test_func"
    symbol = call.scope.symbol_table.lookup("test_func")
    assert isinstance(symbol, RoutineSymbol)
    assert isinstance(symbol.datatype, ScalarType)
    assert symbol.datatype is function.return_symbol.datatype
