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


''' Performs py.test tests on the support for calls in the fparser2
    PSyIR front-end '''

from __future__ import absolute_import
import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from psyclone.psyir.frontend import fparser2
from psyclone.psyir.frontend.fparser2 import Fparser2Reader, \
    get_literal_precision
from psyclone.psyir.symbols import ScalarType, DataSymbol, INTEGER_TYPE, \
    RoutineSymbol, UnresolvedInterface
from psyclone.psyir.nodes import Node, Literal, CodeBlock, Schedule, Call, \
    Reference, BinaryOperation
from psyclone.errors import InternalError


@pytest.mark.usefixtures("f2008_parser")
def test_call_noargs():
    '''Test that fparser2 transforms a Fortran subroutine call with no
    arguments into the equivalent PSyIR Call node. Also test that a
    new RoutineSymbol is added to the symbol table (with an unresolved
    interface), if one does not already exist.

    '''
    reader = FortranStringReader(" call kernel()")
    astmt = Fortran2003.Call_Stmt(reader)
    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [astmt])
    assert not fake_parent.walk(CodeBlock)
    call_node = fake_parent.children[0]
    assert isinstance(call_node, Call)
    routine_symbol = call_node.routine
    assert isinstance(routine_symbol, RoutineSymbol)
    assert isinstance(routine_symbol.interface, UnresolvedInterface)
    assert routine_symbol.name == "kernel"
    assert routine_symbol in call_node.scope.symbol_table.symbols
    assert not call_node.children
    assert (str(call_node)) == "Call[name='kernel']"


def test_call_args(f2008_parser):
    '''Test that fparser2 transforms a Fortran subroutine call with
    arguments into the equivalent PSyIR Call node. Also test that an
    existing RoutineSymbol is used if one with the same name as the
    call name already exists in the symbol table.

    '''

    *** TEST EXISTING ROUTINESYMBOL AND EXISTING SYMBOL WITH WRONG TYPE ***
    test_code = (
        "subroutine test()\n"
        "real :: a,b\n"
        "  call kernel(1.0, a, (a+b)*2.0, name=\"roo\")\n"
        "end subroutine")
    reader = FortranStringReader(test_code)
    ptree = f2008_parser(reader)
    processor = Fparser2Reader()
    sched = processor.generate_schedule("test", ptree)
    call_node = sched.children[0]
    assert isinstance(call_node, Call)
    assert call_node.routine.name == "kernel"
    assert (str(call_node)) == "Call[name='kernel']"
    assert len(call_node.children) == 4
    assert isinstance(call_node.children[0], Literal)
    assert call_node.children[0].value == "1.0"
    assert isinstance(call_node.children[1], Reference)
    assert call_node.children[1].name == "a"
    assert isinstance(call_node.children[2], BinaryOperation)
    assert isinstance(call_node.children[3], CodeBlock)
