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
from psyclone.psyir.backend.base import VisitorError
from psyclone.psyir.backend.fortran import get_intent, get_dims, get_kind, \
    FortranPSyIRVisitor
from psyclone.psyGen import Symbol, Fparser2ASTProcessor, Node, CodeBlock
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


def create_schedule(code):
    '''Utility function that returns a PSyIR tree from Fortran
    code using fparser2 and Fparser2ASTProcessor.

    :param str code: Fortran code.

    :returns: PSyIR tree representing the Fortran code.
    :rtype: Subclass of :py:class:`psyclone.psyGen.Node`

    '''
    reader = FortranStringReader(code)
    f2003_parser = ParserFactory().create(std="f2003")
    parse_tree = f2003_parser(reader)

    # Generate PSyIR schedule from fparser2 parse tree
    processor = Fparser2ASTProcessor()
    schedule = processor.generate_schedule("tmp", parse_tree)

    return schedule

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
    schedule = create_schedule(code)

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

    
def test_FortranPSyIRVisitor_nemokern():
    '''Check the FortranPSyIRVisitor class nemokern method prints the
    class information and calls any children. This method is used to
    output nothing for a NemoKern object and simply call its children
    as NemoKern is a collection of PSyIR nodes so needs no
    output itself.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  a = b/c\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)

    # add a NemoKern object to the tree
    from psyclone.nemo import NemoKern
    nemo_kern = NemoKern(schedule.children, None, parent=schedule)
    schedule.children = [nemo_kern]

    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranPSyIRVisitor()
    result = fvisitor.visit(schedule)
    assert (
        "  subroutine tmp()\n"
        "\n"
        "    a=b/c\n"
        "\n"
        "  end subroutine tmp\n") in result


def test_FortranPSyIRVisitor_kenelschedule():
    '''Check the FortranPSyIRVisitor class nemokern method prints the
    class information and calls any children. This method is used to
    output nothing for a NemoKern object and simply call its children
    as NemoKern is a collection of PSyIR nodes so needs no
    output itself.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,b,c)\n"
        "  real, intent(out) :: a(:)\n"
        "  real, intent(in) :: b(:)\n"
        "  integer, intent(in) :: c\n"
        "  a = b/c\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)
    
    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranPSyIRVisitor()
    result = fvisitor.visit(schedule)
    print result
    assert (
        "  subroutine tmp(a,b,c)\n"
        "    real(r_def), dimension(:), intent(out) :: a\n"
        "    integer(i_def), intent(in) :: c\n"
        "    real(r_def), dimension(:), intent(in) :: b\n"
        "\n"
        "    a=b/c\n"
        "\n"
        "  end subroutine tmp\n") in result

# assignment and binaryoperation are already checked within previous
# tests


def test_FortranPSyIRVisitor_reference():
    '''Check the FortranPSyIRVisitor class reference method prints the
    appropriate information (the name of the reference it points to).
    Also check the method raises an exception if it has children as
    this is not expected.

    '''
    # Generate fparser2 parse tree from Fortran code. The line of
    # interest is a(n) = 0.0. The additional a=1 line is added to get
    # round a bug in the parser.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = 1\n"
        "    a(n) = 0.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)
    
    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranPSyIRVisitor()
    result = fvisitor.visit(schedule)
    print result
    assert (
        "module tmp_mod\n"
        "  use constants_mod, only : r_def, i_def\n"
        "  implicit none\n"
        "  contains\n"
        "  subroutine tmp(a,n)\n"
        "    real(r_def), dimension(n), intent(out) :: a\n"
        "    integer(i_def), intent(in) :: n\n"
        "\n"
        "    a=1\n"
        "    a(n)=0.0\n"
        "\n"
        "  end subroutine tmp\n"
        "end module tmp_mod") in result

    # Now add a child to the reference node
    reference = schedule.children[1].children[0].children[0]
    reference.children = ["hello"]
    
    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as excinfo:
        result = fvisitor.visit(schedule)
    assert "PSyIR Reference node should not have any children." in str(excinfo)


def test_FortranPSyIRVisitor_array():
    '''Check the FortranPSyIRVisitor class array method correctly prints
    out the Fortran representation of an array

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n,n,n)\n"
        "    a = 1\n"
        "    a(2,n,:) = 0.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)
    
    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranPSyIRVisitor()
    result = fvisitor.visit(schedule)
    assert "a(2,n,:)=0.0" in result

# literal is already checked within previous tests

# TODO IFBLOCK

def test_FortranPSyIRVisitor_unaryoperation():
    '''Check the FortranPSyIRVisitor class unary_operation method
    correctly prints out the Fortran representation. Uses -1 as the
    example.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = -1\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)
    
    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranPSyIRVisitor()
    result = fvisitor.visit(schedule)
    assert "a=-1" in result


def test_FortranPSyIRVisitor_return():
    '''Check the FortranPSyIRVisitor class return method
    correctly prints out the Fortran representation.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  return\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)
    
    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranPSyIRVisitor()
    result = fvisitor.visit(schedule)
    assert "    return\n" in result


def test_FortranPSyIRVisitor_codeblock():
    '''Check the FortranPSyIRVisitor class codeblock method correctly
    prints out the Fortran code contained within it.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  integer :: a\n"
        "  a=1\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)
    
    code1 = (
        "print *, 'I am a code block'\n"
        "print *, 'with more than one line'\n")
    from fparser.two.parser import ParserFactory
    import fparser.two.Fortran2003 as Fortran2003
    from fparser.api import get_reader
    _ = ParserFactory().create(std="f2003")
    reader = get_reader(code1)
    statements = Fortran2003.Execution_Part(reader)    
    code_block = CodeBlock([statements], parent=schedule)
    schedule.addchild(code_block)

    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranPSyIRVisitor()
    result = fvisitor.visit(schedule)

    assert (
        "    a=1\n"
        "PRINT *, 'I am a code block'\n"
        "    PRINT *, 'with more than one line'\n" in result)

