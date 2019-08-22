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
from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader
import fparser.two.Fortran2003 as Fortran2003
from fparser.api import get_reader
from psyclone.psyir.backend.base import VisitorError
from psyclone.psyir.backend.fortran import gen_intent, gen_dims, gen_kind, \
    FortranWriter
from psyclone.psyGen import Symbol, Fparser2ASTProcessor, Node, CodeBlock


def test_gen_intent():
    '''Check the gen_intent function produces the expected intent
    strings.

    '''
    symbol = Symbol("dummy", "integer",
                    interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    assert gen_intent(symbol) is None
    symbol = Symbol("dummy", "integer",
                    interface=Symbol.Argument(Symbol.Access.READ))
    assert gen_intent(symbol) == "in"
    symbol = Symbol("dummy", "integer",
                    interface=Symbol.Argument(Symbol.Access.WRITE))
    assert gen_intent(symbol) == "out"
    symbol = Symbol("dummy", "integer",
                    interface=Symbol.Argument(Symbol.Access.READWRITE))
    assert gen_intent(symbol) == "inout"


def test_gen_intent_error(monkeypatch):
    '''Check the gen_intent function raises an exception if an unsupported
    access type is found.

    '''
    symbol = Symbol("dummy", "integer",
                    interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    monkeypatch.setattr(symbol.interface, "_access", "UNSUPPORTED")
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_intent(symbol)
    assert "Unsupported access ''UNSUPPORTED'' found." in str(excinfo)


def test_gen_dims():
    '''Check the gen_dims function produces the expected dimension
    strings.

    '''
    arg = Symbol("arg", "integer",
                 interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    symbol = Symbol("dummy", "integer", shape=[arg, 2, None],
                    interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    assert gen_dims(symbol) == ["arg", "2", ":"]


def test_gen_dims_error(monkeypatch):
    '''Check the gen_dims function raises an exception if a symbol shape
    entry is not supported.

    '''
    symbol = Symbol("dummy", "integer",
                    interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    monkeypatch.setattr(symbol, "_shape", ["invalid"])
    with pytest.raises(NotImplementedError) as excinfo:
        _ = gen_dims(symbol)
    assert "unsupported gen_dims index 'invalid'" in str(excinfo)


def test_gen_kind():
    '''Check the gen_kind function produces the expected kind values. Note
    these are currently hardcoded to support the LFRic API. Issue #375
    captures this problem.

    '''
    int_symbol = Symbol(
        "dummy1", "integer",
        interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    real_symbol = Symbol(
        "dummy2", "real",
        interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    logical_symbol = Symbol(
        "dummy3", "boolean",
        interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))

    assert gen_kind(int_symbol) == "i_def"
    assert gen_kind(real_symbol) == "r_def"
    assert gen_kind(logical_symbol) is None


def test_fw_gen_declaration():
    '''Check the FortranWriter class gen_declaration method produces
    the expected declarations.

    '''
    fvisitor = FortranWriter()

    # Basic entry
    symbol = Symbol("dummy1", "integer")
    result = fvisitor.gen_declaration(symbol)
    assert result == "integer(i_def) :: dummy1\n"

    # Array with intent
    symbol = Symbol("dummy2", "integer", shape=[2, None, 2],
                    interface=Symbol.Argument(access=Symbol.Access.READ))
    result = fvisitor.gen_declaration(symbol)
    assert result == "integer(i_def), dimension(2,:,2), intent(in) :: dummy2\n"

    # Array with unknown intent
    symbol = Symbol("dummy2", "integer", shape=[2, None, 2],
                    interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    result = fvisitor.gen_declaration(symbol)
    assert result == "integer(i_def), dimension(2,:,2) :: dummy2\n"

    # Constant
    symbol = Symbol("dummy3", "integer", constant_value=10)
    result = fvisitor.gen_declaration(symbol)
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


def test_fw_exception():
    '''Check the FortranWriter class instance raises an exception if an
    unsupported PSyIR node is found.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  integer :: a,b,c\n"
        "  a = b/c\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)

    # pylint: disable=abstract-method
    # modify the reference to b to be something unsupported
    class Unsupported(Node):
        '''A PSyIR node that will not be supported by the Fortran visitor.'''
    # pylint: enable=abstract-method

    unsupported = Unsupported()
    assignment = schedule.children[0]
    binary_operation = assignment.children[1]
    assignment.children[1] = unsupported
    unsupported.children = binary_operation.children

    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranWriter()
    with pytest.raises(VisitorError) as excinfo:
        _ = fvisitor(schedule)
    assert "Unsupported node 'Unsupported' found" in str(excinfo)


def test_fw_kernelschedule(monkeypatch):
    '''Check the FortranWriter class outputs correct code when a
    KernelSchedule node is found. Also tests that an exception is
    raised if KernelSchedule.name does not have a value.

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
    fvisitor = FortranWriter()
    result = fvisitor(schedule)

    assert(
        "module tmp_mod\n"
        "  use constants_mod, only : r_def, i_def\n"
        "  implicit none\n"
        "  contains\n"
        "  subroutine tmp(a,b,c)\n"
        "    real(r_def), dimension(:), intent(out) :: a\n"
        "    real(r_def), dimension(:), intent(in) :: b\n"
        "    integer(i_def), intent(in) :: c\n"
        "\n"
        "    a=b / c\n"
        "\n"
        "  end subroutine tmp\n"
        "end module tmp_mod") in result

    monkeypatch.setattr(schedule, "_name", None)
    with pytest.raises(VisitorError) as excinfo:
        _ = fvisitor(schedule)
    assert "Expected node name to have a value." in str(excinfo)

# assignment and binaryoperation (not intrinsics) are already checked
# within previous tests


def test_fw_binaryoperator():
    '''Check the FortranWriter class binary_operation method correctly
    prints out the Fortran representation of an intrinsic. Uses sign
    as the example.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = sign(1.0,1.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)

    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranWriter()
    result = fvisitor(schedule)
    assert "a=SIGN(1.0, 1.0)" in result


def test_fw_binaryoperator_unknown(monkeypatch):
    '''Check the FortranWriter class binary_operation method raises an
    exception if an unknown binary operator is found.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = sign(1.0,1.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)
    # Remove sign() from the list of supported binary operators
    monkeypatch.delitem(Fparser2ASTProcessor.binary_operators, "sign")
    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranWriter()
    with pytest.raises(VisitorError) as excinfo:
        _ = fvisitor(schedule)
    assert "Unexpected binary op" in str(excinfo)


def test_fw_naryopeator():
    ''' Check that the FortranWriter class nary_operation method correctly
    prints out the Fortran representation of an intrinsic.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a\n"
        "    a = max(1.0,1.0,2.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)

    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranWriter()
    result = fvisitor(schedule)
    assert "a=MAX(1.0, 1.0, 2.0)" in result


def test_fw_naryopeator_unknown(monkeypatch):
    ''' Check that the FortranWriter class nary_operation method raises
    the expected error if it encounters an unknown operator.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a\n"
        "    a = max(1.0,1.0,2.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)
    # Remove max() from the list of supported nary operators
    monkeypatch.delitem(Fparser2ASTProcessor.nary_operators, "max")
    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranWriter()
    with pytest.raises(VisitorError) as err:
        _ = fvisitor(schedule)
    assert "Unexpected N-ary op" in str(err)


def test_fw_reference():
    '''Check the FortranWriter class reference method prints the
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
    fvisitor = FortranWriter()
    result = fvisitor(schedule)

    # The asserts need to be split as the declaration order can change
    # between different versions of Psython.
    assert (
        "module tmp_mod\n"
        "  use constants_mod, only : r_def, i_def\n"
        "  implicit none\n"
        "  contains\n"
        "  subroutine tmp(a,n)\n"
        "    integer(i_def), intent(in) :: n\n"
        "    real(r_def), dimension(n), intent(out) :: a\n"
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
        result = fvisitor(schedule)
    assert "PSyIR Reference node should not have any children." in str(excinfo)


def test_fw_array():
    '''Check the FortranWriter class array method correctly prints
    out the Fortran representation of an array

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n,n,n)\n"
        "    a(2,n,3) = 0.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)

    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranWriter()
    result = fvisitor(schedule)
    assert "a(2,n,3)=0.0" in result

# literal is already checked within previous tests


def test_fw_ifblock():
    '''Check the FortranWriter class ifblock method
    correctly prints out the Fortran representation.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(inout) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    if (n.gt.2) then\n"
        "      n=n+1\n"
        "    end if\n"
        "    if (n.gt.4) then\n"
        "      a = -1\n"
        "    else\n"
        "      a = 1\n"
        "    end if\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)

    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranWriter()
    result = fvisitor(schedule)
    assert (
        "    if (n > 2) then\n"
        "      n=n + 1\n"
        "    end if\n"
        "    if (n > 4) then\n"
        "      a=-1\n"
        "    else\n"
        "      a=1\n"
        "    end if\n") in result


def test_fw_loop():
    '''Check the FortranWriter class loop method
    correctly prints out the Fortran representation.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  integer :: i, sum\n"
        "  sum = 0\n"
        "  do i = 1, 20, 2\n"
        "    sum = sum + i\n"
        "  end do\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)

    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranWriter()
    result = fvisitor(schedule)
    assert "do i = 1, 20, 2\n" in result


def test_fw_unaryoperator():
    '''Check the FortranWriter class unary_operation method
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
    fvisitor = FortranWriter()
    result = fvisitor(schedule)
    assert "a=-1" in result


def test_fw_unaryoperator2():
    '''Check the FortranWriter class unary_operation method correctly
    prints out the Fortran representation of an intrinsic. Uses sin as
    the example.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = sin(1.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)

    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranWriter()
    result = fvisitor(schedule)
    assert "a=SIN(1.0)" in result


def test_fw_unaryoperator_unknown(monkeypatch):
    '''Check the FortranWriter class unary_operation method raises an
    exception if an unknown unary operator is found.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = sin(1.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)
    # Remove sin() from the dict of unary operators
    monkeypatch.delitem(Fparser2ASTProcessor.unary_operators, "sin")
    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranWriter()
    with pytest.raises(VisitorError) as excinfo:
        _ = fvisitor(schedule)
    assert "Unexpected unary op" in str(excinfo)


def test_fw_return():
    '''Check the FortranWriter class return method
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
    fvisitor = FortranWriter()
    result = fvisitor(schedule)
    assert "    return\n" in result


def test_fw_codeblock_1():
    '''Check the FortranWriter class codeblock method correctly
    prints out the Fortran code contained within it.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  integer :: a\n"
        "  a=1\n"
        "  print *,\"I am a code block\"\n"
        "  print *,\"with more than one line\"\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)

    # Check a code block exists in the schedule
    assert schedule.walk(CodeBlock)

    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranWriter()
    result = fvisitor(schedule)

    assert (
        "    a=1\n"
        "    PRINT *, \"I am a code block\"\n"
        "    PRINT *, \"with more than one line\"\n" in result)


@pytest.mark.xfail(reason="issue #388. Code blocks add space and newline.")
def test_fw_codeblock_2():
    '''Check the FortranWriter class array method correctly prints out the
    Fortran representation when there is a code block that is part of
    a line (not a whole line). In this case the ":" in the array
    access is a code block.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n,n,n)\n"
        "    a(2,n,:) = 0.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)

    # Check a code block exists in the schedule
    assert schedule.walk(CodeBlock)

    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranWriter()
    result = fvisitor(schedule)
    assert "a(2,n,:)=0.0" in result


def test_nemoinvokeschedule(parser):
    ''' xxx '''
    from psyclone.nemo import NemoInvokeSchedule
    code = (
        "program test\n"
        "  a=1\n"
        "end program test\n")
    from fparser.common.readfortran import FortranStringReader
    from psyclone.psyGen import PSyFactory
    reader = FortranStringReader(code)
    prog = parser(reader)
    psy = PSyFactory(api="nemo").create(prog)
    schedule = psy.invokes.invoke_list[0].schedule
    assert isinstance(schedule, NemoInvokeSchedule)
    fvisitor = FortranWriter()
    result = fvisitor(schedule)
    assert "a=1\n" in result


def test_fw_nemokern():
    '''Check the FortranWriter class nemokern method prints the
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
        "  integer :: a,b,c\n"
        "  a = b/c\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)

    # add a NemoKern object to the tree
    from psyclone.nemo import NemoKern
    nemo_kern = NemoKern(schedule.children, None, parent=schedule)
    schedule.children = [nemo_kern]

    # Generate Fortran from the PSyIR schedule
    fvisitor = FortranWriter()
    result = fvisitor(schedule)
    assert(
        "  subroutine tmp()\n"
        "    integer(i_def) :: a\n"
        "    integer(i_def) :: b\n"
        "    integer(i_def) :: c\n"
        "\n"
        "    a=b / c\n"
        "\n"
        "  end subroutine tmp\n") in result


# nemoimplicitloop_node ***
