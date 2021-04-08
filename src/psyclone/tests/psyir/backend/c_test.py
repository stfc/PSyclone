# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
# Author S. Siso, STFC Daresbury Lab
# Modified by A. R. Porter and R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclone.psyir.backend.c module'''

from __future__ import absolute_import

import pytest

from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.backend.c import CWriter
from psyclone.psyir.nodes import Node, CodeBlock, Assignment, \
    Reference, Return, ArrayReference, Literal, UnaryOperation, \
    BinaryOperation, Schedule
from psyclone.psyir.symbols import DataSymbol, ArgumentInterface, \
    ArrayType, REAL_TYPE, INTEGER_TYPE, CHARACTER_TYPE, BOOLEAN_TYPE


def test_cw_gen_declaration():
    '''Check the CWriter class gen_declaration method produces
    the expected declarations.

    '''
    cwriter = CWriter()

    # Basic entries
    symbol = DataSymbol("dummy1", INTEGER_TYPE)
    result = cwriter.gen_declaration(symbol)
    assert result == "int dummy1"

    symbol = DataSymbol("dummy1", CHARACTER_TYPE)
    result = cwriter.gen_declaration(symbol)
    assert result == "char dummy1"

    symbol = DataSymbol("dummy1", BOOLEAN_TYPE)
    result = cwriter.gen_declaration(symbol)
    assert result == "bool dummy1"

    # Array argument
    array_type = ArrayType(REAL_TYPE, [2, ArrayType.Extent.ATTRIBUTE, 2])
    symbol = DataSymbol("dummy2", array_type,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READ))
    result = cwriter.gen_declaration(symbol)
    assert result == "double * restrict dummy2"

    # Array with unknown access
    array_type = ArrayType(INTEGER_TYPE, [2, ArrayType.Extent.ATTRIBUTE, 2])
    symbol = DataSymbol("dummy2", array_type,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    result = cwriter.gen_declaration(symbol)
    assert result == "int * restrict dummy2"

    # Check invalid datatype produces and error
    symbol._datatype = "invalid"
    with pytest.raises(NotImplementedError) as error:
        _ = cwriter.gen_declaration(symbol)
    assert "Could not generate the C definition for the variable 'dummy2', " \
        "type 'invalid' is currently not supported." in str(error.value)


def test_cw_gen_local_variable(monkeypatch):
    '''Check the CWriter class gen_local_variable method produces
    the expected declarations.

    '''
    cwriter = CWriter()

    monkeypatch.setattr(cwriter, "gen_declaration",
                        lambda x: "<declaration>")

    # Local variables are declared as single statements
    symbol = DataSymbol("dummy1", INTEGER_TYPE)
    result = cwriter.gen_local_variable(symbol)
    # Result should include the mocked gen_declaration and ';\n'
    assert result == "<declaration>;\n"


def test_cw_exception():
    '''Check the CWriter class instance raises an exception if an
    unsupported PSyIR node is found.

    '''
    # pylint: disable=abstract-method
    # Define a Node which will be unsupported by the visitor
    class Unsupported(Node):
        '''A PSyIR node that will not be supported by the C visitor.'''
    # pylint: enable=abstract-method

    unsupported = Unsupported()

    cwriter = CWriter()
    with pytest.raises(VisitorError) as excinfo:
        _ = cwriter(unsupported)
    assert "Unsupported node 'Unsupported' found" in str(excinfo.value)


def test_cw_literal():
    '''Check the CWriter class literal method correctly prints
    out the C representation of a Literal.

    '''

    cwriter = CWriter()

    lit = Literal('1', INTEGER_TYPE)
    assert cwriter(lit) == '1'

    # Test that scientific notation is output correctly
    lit = Literal("3e5", REAL_TYPE, None)
    assert cwriter(lit) == '3e5'


def test_cw_assignment():
    '''Check the CWriter class assignment method generate the appropriate
    output.

    '''
    assignment = Assignment.create(Reference(DataSymbol('a', REAL_TYPE)),
                                   Reference(DataSymbol('b', REAL_TYPE)))
    # Generate C from the PSyIR schedule
    cwriter = CWriter()
    result = cwriter(assignment)
    assert result == "a = b;\n"


def test_cw_array():
    '''Check the CWriter class array method correctly prints
    out the C representation of an array.

    '''
    cwriter = CWriter()

    symbol = DataSymbol('a', REAL_TYPE)
    arr = ArrayReference(symbol)
    lit = Literal('0.0', REAL_TYPE)
    assignment = Assignment.create(arr, lit)

    # An array without any children (dimensions) should produce an error.
    with pytest.raises(VisitorError) as excinfo:
        result = cwriter(assignment)
    assert "Arrays must have at least 1 dimension but found node: '" \
        in str(excinfo.value)

    # Dimensions can be references, literals or operations
    arr.addchild(Reference(DataSymbol('b', INTEGER_TYPE)))
    arr.addchild(Literal('1', INTEGER_TYPE))
    uop = UnaryOperation.create(UnaryOperation.Operator.MINUS,
                                Literal('2', INTEGER_TYPE))
    arr.addchild(uop)

    result = cwriter(assignment)
    # Results is reversed and flatten (row-major 1D)
    # dimensions are called <name>LEN<dimension> by convention
    assert result == "a[(-2) * aLEN2 * aLEN1 + 1 * aLEN1 + b] = 0.0;\n"


def test_cw_ifblock():
    '''Check the CWriter class ifblock method correctly prints out the
    C representation.

    '''
    from psyclone.psyir.nodes import IfBlock

    # Try with just an IfBlock node
    ifblock = IfBlock()
    cwriter = CWriter()
    with pytest.raises(VisitorError) as err:
        _ = cwriter(ifblock)
    assert("IfBlock malformed or incomplete. It should have "
           "at least 2 children, but found 0." in str(err.value))

    # Add the if condition
    ifblock.addchild(Reference(DataSymbol('a', REAL_TYPE)))
    with pytest.raises(VisitorError) as err:
        _ = cwriter(ifblock)
    assert("IfBlock malformed or incomplete. It should have "
           "at least 2 children, but found 1." in str(err.value))

    # Fill the if_body and else_body
    ifblock.addchild(Schedule(parent=ifblock))
    ifblock.addchild(Schedule(parent=ifblock))
    ifblock.if_body.addchild(Return(parent=ifblock.if_body))

    condition = Reference(DataSymbol('b', REAL_TYPE))
    then_content = [Return()]
    else_content = [Return()]
    ifblock2 = IfBlock.create(condition, then_content, else_content)
    ifblock.else_body.addchild(ifblock2)

    result = cwriter(ifblock)
    assert result == (
        "if (a) {\n"
        "  return;\n"
        "} else {\n"
        "  if (b) {\n"
        "    return;\n"
        "  } else {\n"
        "    return;\n"
        "  }\n"
        "}\n")


def test_cw_return():
    '''Check the CWriter class return method correctly prints out the
    C representation.

    '''
    cwriter = CWriter()
    result = cwriter(Return())
    assert "return;\n" in result


def test_cw_codeblock():
    '''Check the CWriter class codeblock method raises the expected
    error.
    '''

    cblock = CodeBlock([], "dummy")
    cwriter = CWriter()

    with pytest.raises(VisitorError) as error:
        _ = cwriter(cblock)
    assert "CodeBlocks can not be translated to C." in str(error.value)


def test_cw_unaryoperator():
    '''Check the CWriter class unary_operation method correctly prints out
    the C representation of any given UnaryOperation.

    '''
    cwriter = CWriter()

    # Test UnaryOperation without children.
    unary_operation = UnaryOperation(UnaryOperation.Operator.MINUS)
    with pytest.raises(VisitorError) as err:
        _ = cwriter(unary_operation)
    assert("UnaryOperation malformed or incomplete. It should have "
           "exactly 1 child, but found 0." in str(err.value))

    # Add child
    ref1 = Literal("a", CHARACTER_TYPE, unary_operation)
    unary_operation.addchild(ref1)
    assert cwriter(unary_operation) == '(-a)'

    # Test all supported Operators
    test_list = ((UnaryOperation.Operator.PLUS, '(+a)'),
                 (UnaryOperation.Operator.MINUS, '(-a)'),
                 (UnaryOperation.Operator.SQRT, 'sqrt(a)'),
                 (UnaryOperation.Operator.NOT, '(!a)'),
                 (UnaryOperation.Operator.COS, 'cos(a)'),
                 (UnaryOperation.Operator.SIN, 'sin(a)'),
                 (UnaryOperation.Operator.TAN, 'tan(a)'),
                 (UnaryOperation.Operator.ACOS, 'acos(a)'),
                 (UnaryOperation.Operator.ASIN, 'asin(a)'),
                 (UnaryOperation.Operator.ATAN, 'atan(a)'),
                 (UnaryOperation.Operator.ABS, 'abs(a)'),
                 (UnaryOperation.Operator.REAL, '(float)a'))

    for operator, expected in test_list:
        unary_operation._operator = operator
        assert cwriter(unary_operation) in expected

    # Test that an unsupported operator raises an error
    class Unsupported(object):
        # pylint: disable=missing-docstring
        pass
    unary_operation._operator = Unsupported
    with pytest.raises(NotImplementedError) as err:
        _ = cwriter(unary_operation)
    assert "The C backend does not support the '" in str(err.value)
    assert "' operator." in str(err.value)


def test_cw_binaryoperator():
    '''Check the CWriter class binary_operation method correctly
    prints out the C representation of any given BinaryOperation.

    '''
    cwriter = CWriter()

    # Test UnaryOperation without children.
    binary_operation = BinaryOperation(BinaryOperation.Operator.ADD)
    with pytest.raises(VisitorError) as err:
        _ = cwriter(binary_operation)
    assert("BinaryOperation malformed or incomplete. It should have "
           "exactly 2 children, but found 0." in str(err.value))

    # Test with children
    ref1 = Reference(DataSymbol("a", REAL_TYPE))
    ref2 = Reference(DataSymbol("b", REAL_TYPE))
    binary_operation = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                              ref1, ref2)
    assert cwriter(binary_operation) == '(a + b)'

    # Test all supported Operators
    test_list = ((BinaryOperation.Operator.ADD, '(a + b)'),
                 (BinaryOperation.Operator.SUB, '(a - b)'),
                 (BinaryOperation.Operator.MUL, '(a * b)'),
                 (BinaryOperation.Operator.DIV, '(a / b)'),
                 (BinaryOperation.Operator.REM, '(a % b)'),
                 (BinaryOperation.Operator.POW, 'pow(a, b)'),
                 (BinaryOperation.Operator.EQ, '(a == b)'),
                 (BinaryOperation.Operator.NE, '(a != b)'),
                 (BinaryOperation.Operator.GT, '(a > b)'),
                 (BinaryOperation.Operator.GE, '(a >= b)'),
                 (BinaryOperation.Operator.LT, '(a < b)'),
                 (BinaryOperation.Operator.LE, '(a <= b)'),
                 (BinaryOperation.Operator.AND, '(a && b)'),
                 (BinaryOperation.Operator.OR, '(a || b)'),
                 (BinaryOperation.Operator.SIGN, 'copysign(a, b)'))

    for operator, expected in test_list:
        binary_operation._operator = operator
        assert cwriter(binary_operation) == expected

    # Test that an unsupported operator raises a error
    class Unsupported(object):
        '''Dummy class'''
        def __init__(self):
            pass
    binary_operation._operator = Unsupported
    with pytest.raises(VisitorError) as err:
        _ = cwriter(binary_operation)
    assert "The C backend does not support the '" in str(err.value)
    assert "' operator." in str(err.value)


def test_cw_loop():
    '''Tests writing out a Loop node in C. It parses Fortran code
    and outputs it as C. Note that this is atm a literal translation,
    the loops are not functionally identical to Fortran, see TODO #523.

    '''
    from psyclone.tests.utilities import create_schedule

    # Generate PSyIR from Fortran code.
    code = '''
        module test
        contains
        subroutine tmp()
          integer :: i, a
          integer, dimension(:) :: b
          do i = 1, 20, 2
            a = 2 * i
          enddo
        end subroutine tmp
        end module test'''
    schedule = create_schedule(code, "tmp")

    cvisitor = CWriter()
    result = cvisitor(schedule[0])
    correct = '''for(i=1; i<=20; i+=2)
{
  a = (2 * i);
}'''
    result = cvisitor(schedule[0])
    assert correct in result


def test_cw_size():
    ''' Check the CWriter class SIZE method raises the expected error since
    there is no C equivalent. '''
    cwriter = CWriter()
    arr = ArrayReference(DataSymbol('a', INTEGER_TYPE))
    lit = Literal('1', INTEGER_TYPE)
    size = BinaryOperation.create(BinaryOperation.Operator.SIZE, arr, lit)
    lhs = Reference(DataSymbol('length', INTEGER_TYPE))
    assignment = Assignment.create(lhs, size)

    with pytest.raises(VisitorError) as excinfo:
        cwriter(assignment)
    assert ("C backend does not support the 'Operator.SIZE' operator"
            in str(excinfo.value))
