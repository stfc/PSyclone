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
# Author S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclone.psyir.backend.c module'''

import pytest
from psyclone.psyir.backend.base import VisitorError
from psyclone.psyir.backend.c import CWriter
from psyclone.psyGen import Symbol, Node, CodeBlock, Assignment, Reference, \
    Return, Array, Literal, UnaryOperation, BinaryOperation, Schedule


def test_cw_gen_declaration():
    '''Check the CWriter class gen_declaration method produces
    the expected declarations.

    '''
    cwriter = CWriter()

    # Basic entries
    symbol = Symbol("dummy1", "integer")
    result = cwriter.gen_declaration(symbol)
    assert result == "int dummy1"

    symbol = Symbol("dummy1", "character")
    result = cwriter.gen_declaration(symbol)
    assert result == "char dummy1"

    symbol = Symbol("dummy1", "boolean")
    result = cwriter.gen_declaration(symbol)
    assert result == "bool dummy1"

    # Array argument
    symbol = Symbol("dummy2", "real", shape=[2, None, 2],
                    interface=Symbol.Argument(access=Symbol.Access.READ))
    result = cwriter.gen_declaration(symbol)
    assert result == "double * restrict dummy2"

    # Array with unknown intent
    symbol = Symbol("dummy2", "integer", shape=[2, None, 2],
                    interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    result = cwriter.gen_declaration(symbol)
    assert result == "int * restrict dummy2"


def test_cw_gen_local_variable(monkeypatch):
    '''Check the CWriter class gen_local_variable method produces
    the expected declarations.

    '''
    cwriter = CWriter()

    monkeypatch.setattr(cwriter, "gen_declaration",
                        lambda x: "<declaration>")

    # Local variables are declared as single statements
    symbol = Symbol("dummy1", "integer")
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
    assert "Unsupported node 'Unsupported' found" in str(excinfo)


def test_cw_literal():
    '''Check the CWriter class literal method correctly prints
    out the C representation of a Literal.

    '''

    cwriter = CWriter()

    lit = Literal('1')
    assert cwriter(lit) == '1'

    # Test that D scientific notation is replaced by 'e'
    lit = Literal("3e5", None)
    assert cwriter(lit) == '3e5'
    lit = Literal("3d5", None)
    assert cwriter(lit) == '3e5'
    lit = Literal("3D5", None)
    assert cwriter(lit) == '3e5'
    lit = Literal("3D+5", None)
    assert cwriter(lit) == '3e+5'


def test_cw_assignment_and_reference():
    '''Check the CWriter class assignment and reference methods generate
    the appropriate output. Also check that a reference visit raises an
    exception if it has children as this is not expected.

    '''

    assignment = Assignment()
    assignment.addchild(Reference('a', parent=assignment))
    assignment.addchild(Reference('b', parent=assignment))

    # Generate C from the PSyIR schedule
    cwriter = CWriter()
    result = cwriter(assignment)
    assert result == "a = b;\n"

    # Now add a child to the reference node
    assignment.lhs.addchild(Node(parent=assignment))

    # Generate C from the PSyIR schedule
    with pytest.raises(VisitorError) as excinfo:
        result = cwriter(assignment)
    assert  "Expecting a Reference with no children but found: " \
        in str(excinfo)


def test_cw_array():
    '''Check the CWriter class array method correctly prints
    out the C representation of an array.

    '''
    cwriter = CWriter()

    assignment = Assignment()
    a = Array('a', parent=assignment)
    b = Literal('0.0', parent=assignment)
    assignment.addchild(a)
    assignment.addchild(b)

    # An array without any children (dimensions) should produce an error.
    with pytest.raises(VisitorError) as excinfo:
        result = cwriter(assignment)
    assert "Arrays must have at least 1 dimension but found node: '" \
        in str(excinfo)

    # Dimensions can be references, literals or operations
    a.addchild(Reference('b', parent=a))
    a.addchild(Literal('1', parent=a))
    uop = UnaryOperation(UnaryOperation.Operator.MINUS, parent=a)
    uop.addchild(Literal('2', parent=uop))
    a.addchild(uop)

    result = cwriter(assignment)
    # Results is reversed and flatten (row-major 1D)
    # dimensions are called <name>LEN<dimension> by convention
    assert "a[(-2) * aLEN2 * aLEN1 + 1 * aLEN1 + b] = 0.0;\n" == result


def test_cw_ifblock():
    '''Check the CWriter class ifblock method correctly prints out the
    C representation.

    '''
    from psyclone.psyGen import IfBlock

    # Try with just a IfBlock node
    ifblock = IfBlock()
    cwriter = CWriter()
    with pytest.raises(VisitorError) as err:
        _ = cwriter(ifblock)
    assert("IfBlock malformed or incomplete. It should have "
           "at least 2 children, but found 0." in str(err.value))

    # Add the if condition
    ifblock.addchild(Reference('a', parent=ifblock))
    with pytest.raises(VisitorError) as err:
        _ = cwriter(ifblock)
    assert("IfBlock malformed or incomplete. It should have "
           "at least 2 children, but found 1." in str(err.value))

    # Fill the if_body and else_body
    ifblock.addchild(Schedule(parent=ifblock))
    ifblock.addchild(Schedule(parent=ifblock))
    ifblock.if_body.addchild(Return(parent=ifblock.if_body))

    ifblock2 = IfBlock(parent=ifblock.else_body)
    ifblock2.addchild(Reference('b', parent=ifblock2))
    ifblock2.addchild(Schedule(parent=ifblock2))
    ifblock2.if_body.addchild(Return(parent=ifblock2.if_body))
    ifblock2.addchild(Schedule(parent=ifblock2))
    ifblock2.else_body.addchild(Return(parent=ifblock2.else_body))

    ifblock.else_body.addchild(ifblock2)

    result = cwriter(ifblock)
    assert (
        "if (a) {\n"
        "  return;\n"
        "} else {\n"
        "  if (b) {\n"
        "    return;\n"
        "  } else {\n"
        "    return;\n"
        "  }\n"
        "}\n") == result


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

    cblock = CodeBlock([])
    cwriter = CWriter()

    with pytest.raises(VisitorError) as error:
        _ = cwriter(cblock)
    assert "CodeBlocks can not be translated to C." in str(error)


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
    ref1 = Literal("a", unary_operation)
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
                 (UnaryOperation.Operator.REAL, 'float(a)'))

    for operator, expected in test_list:
        unary_operation._operator = operator
        assert cwriter(unary_operation) in expected

    # Test that an unsupported operator raises a error
    # pylint: disable=abstract-method
    class Unsupported():
        '''Dummy class'''
    # pylint: enable=abstract-method
    unary_operation._operator = Unsupported
    with pytest.raises(NotImplementedError) as err:
        _ = cwriter(unary_operation)
    assert "The C backend does not support the '" in str(err)
    assert "' operator." in str(err)


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

    # Add children
    ref1 = Reference("a", binary_operation)
    ref2 = Reference("b", binary_operation)
    binary_operation.addchild(ref1)
    binary_operation.addchild(ref2)
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
    # pylint: disable=abstract-method
    class Unsupported():
        '''Dummy class'''
    # pylint: enable=abstract-method
    binary_operation._operator = Unsupported
    with pytest.raises(VisitorError) as err:
        _ = cwriter(binary_operation)
    assert "The C backend does not support the '" in str(err)
    assert "' operator." in str(err)
