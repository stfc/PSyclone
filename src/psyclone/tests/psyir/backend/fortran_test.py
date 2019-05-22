# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclond.psyir.backend.fortran module'''

import pytest
from psyclone.psyir.backend.fortran import get_intent, get_dims, get_kind, \
    FortranPSyIRVisitor
from psyclone.psyGen import Symbol, Fparser2ASTProcessor, Node
from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader


def test_get_intent():
    '''Check the get_intent function produces the expected intent
    strings.

    '''
    symbol = Symbol("dummy", "integer", scope="global_argument")
    assert get_intent(symbol) is None
    symbol.is_input = True
    assert get_intent(symbol) == "in"
    symbol.is_output = True
    assert get_intent(symbol) == "inout"
    symbol.is_input = False
    assert get_intent(symbol) == "out"


def test_get_dims():
    '''Check the get_dims function produces the expected dimension
    strings.

    '''
    arg = Symbol("arg", "integer", scope="global_argument")
    symbol = Symbol("dummy", "integer", shape=[arg, 2, None],
                    scope="global_argument")
    assert get_dims(symbol) == ["arg", "2", ":"]


def test_get_dims_error(monkeypatch):
    '''Check the get_dims function raises an exception if a symbol shape
    entry is not supported.

    '''
    symbol = Symbol("dummy", "integer", scope="global_argument")
    monkeypatch.setattr(symbol, "_shape", ["invalid"])
    with pytest.raises(NotImplementedError) as excinfo:
        _ = get_dims(symbol)
    assert "unsupported get_dims index 'invalid'" in str(excinfo)


def test_get_kind():
    '''Check the get_kind function produces the expected kind values. Note
    these are currently hardcoded to support the LFRic API. Issue #375
    captures this problem.

    '''
    int_symbol = Symbol("dummy1", "integer", scope="global_argument")
    real_symbol = Symbol("dummy2", "real", scope="global_argument")
    logical_symbol = Symbol("dummy3", "boolean", scope="global_argument")

    assert get_kind(int_symbol) == "i_def"
    assert get_kind(real_symbol) == "r_def"
    assert get_kind(logical_symbol) is None


def test_FortranPSyIRVisitor_get_declaration():
    '''Check the FortranPSyIRVisitor class get_declaration method produces
    the expected declarations.

    '''
    fvisitor = FortranPSyIRVisitor()

    # Basic entry
    symbol = Symbol("dummy1", "integer")
    result = fvisitor.get_declaration(symbol)
    assert result == "integer(i_def) :: dummy1\n"

    # Array with intent
    symbol = Symbol("dummy2", "integer", shape=[2, None, 2],
                    scope="global_argument", is_input=True)
    result = fvisitor.get_declaration(symbol)
    assert result == "integer(i_def), dimension(2,:,2), intent(in) :: dummy2\n"

    # Constant
    symbol = Symbol("dummy3", "integer", constant_value=10)
    result = fvisitor.get_declaration(symbol)
    assert result == "integer(i_def), parameter :: dummy3 = 10\n"


def test_FortranPSyIRVisitor_node():
    '''Check the FortranPSyIRVisitor class node method prints the class
    information and calls any children. This method is used to output
    any unsupported PSyIR nodes in a human readable way.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  a = b/c\n"
        "end subroutine tmp\n"
        "end module test")
    reader = FortranStringReader(code)
    f2003_parser = ParserFactory().create(std="f2003")
    parse_tree = f2003_parser(reader)

    # Generate PSyIR schedule from fparser2 parse tree
    processor = Fparser2ASTProcessor()
    schedule = processor.generate_schedule("tmp", parse_tree)
    schedule.view()

    # modify the reference to b to be something unsupported
    class Unsupported(Node):
        '''A PSyIR node that will not be supported by the Fortran visitor.'''

    unsupported = Unsupported()
    assignment = schedule.children[0]
    binary_operation = assignment.children[1]
    assignment.children[1] = unsupported
    unsupported.children = binary_operation.children
    
    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranPSyIRVisitor()
    result = fvisitor.visit(schedule)

    assert ("    a=    [ Unsupported start ]\n"
            "bc    [ Unsupported end ]\n" in result)

