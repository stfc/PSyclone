# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2023, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

'''Performs pytest tests on the Operation PSyIR node and its
sub-classes.

'''
import pytest

from psyclone.psyir.nodes import UnaryOperation, BinaryOperation, \
    Literal, Reference, Return
from psyclone.psyir.symbols import DataSymbol, INTEGER_SINGLE_TYPE, \
    REAL_SINGLE_TYPE
from psyclone.errors import GenerationError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import check_links
from psyclone.psyir.nodes import colored


# Test BinaryOperation class
def test_binaryoperation_initialization():
    ''' Check the initialization method of the BinaryOperation class works
    as expected.'''

    with pytest.raises(TypeError) as err:
        _ = BinaryOperation("not an operator")
    assert "BinaryOperation operator argument must be of type " \
           "BinaryOperation.Operator but found" in str(err.value)
    bop = BinaryOperation(BinaryOperation.Operator.ADD)
    assert bop._operator is BinaryOperation.Operator.ADD


def test_binaryoperation_operator():
    '''Test that the operator property returns the binaryoperator in the
    binaryoperation.

    '''
    binary_operation = BinaryOperation(BinaryOperation.Operator.ADD)
    assert binary_operation.operator == BinaryOperation.Operator.ADD


def test_binaryoperation_node_str():
    ''' Check the node_str method of the Binary Operation class.'''
    binary_operation = BinaryOperation(BinaryOperation.Operator.ADD)
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    op2 = Literal("1", INTEGER_SINGLE_TYPE)
    binary_operation.addchild(op1)
    binary_operation.addchild(op2)
    coloredtext = colored("BinaryOperation", BinaryOperation._colour)
    assert coloredtext+"[operator:'ADD']" in binary_operation.node_str()


def test_binaryoperation_can_be_printed():
    '''Test that a Binary Operation instance can always be printed (i.e. is
    initialised fully)'''
    binary_operation = BinaryOperation(BinaryOperation.Operator.ADD)
    assert "BinaryOperation[operator:'ADD']" in str(binary_operation)
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    op2 = Literal("2", INTEGER_SINGLE_TYPE)
    binary_operation.addchild(op1)
    binary_operation.addchild(op2)
    # Check the node children are also printed
    assert ("Literal[value:'1', Scalar<INTEGER, SINGLE>]\n"
            in str(binary_operation))
    assert ("Literal[value:'2', Scalar<INTEGER, SINGLE>]"
            in str(binary_operation))


def test_binaryoperation_create():
    '''Test that the create method in the BinaryOperation class correctly
    creates a BinaryOperation instance.

    '''
    lhs = Reference(DataSymbol("tmp1", REAL_SINGLE_TYPE))
    rhs = Reference(DataSymbol("tmp2", REAL_SINGLE_TYPE))
    oper = BinaryOperation.Operator.ADD
    binaryoperation = BinaryOperation.create(oper, lhs, rhs)
    check_links(binaryoperation, [lhs, rhs])
    result = FortranWriter().binaryoperation_node(binaryoperation)
    assert result == "tmp1 + tmp2"


def test_binaryoperation_create_invalid():
    '''Test that the create method in a BinaryOperation class raises the
    expected exception if the provided input is invalid.

    '''
    ref1 = Reference(DataSymbol("tmp1", REAL_SINGLE_TYPE))
    ref2 = Reference(DataSymbol("tmp2", REAL_SINGLE_TYPE))
    add = BinaryOperation.Operator.ADD

    # oper not a BinaryOperation.Operator.
    with pytest.raises(GenerationError) as excinfo:
        _ = BinaryOperation.create("invalid", ref1, ref2)
    assert ("operator argument in create method of BinaryOperation class "
            "should be a PSyIR BinaryOperation Operator but found 'str'."
            in str(excinfo.value))

    # lhs not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = BinaryOperation.create(add, "invalid", ref2)
    assert ("Item 'str' can't be child 0 of 'BinaryOperation'. The valid "
            "format is: 'DataNode, DataNode'.") in str(excinfo.value)

    # rhs not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = BinaryOperation.create(add, ref1, "invalid")
    assert ("Item 'str' can't be child 1 of 'BinaryOperation'. The valid "
            "format is: 'DataNode, DataNode'.") in str(excinfo.value)


def test_binaryoperation_children_validation():
    '''Test that children added to BinaryOperation are validated.
    BinaryOperations accept 2 DataNodes as children.

    '''
    operation = BinaryOperation(BinaryOperation.Operator.ADD)
    literal1 = Literal("1", INTEGER_SINGLE_TYPE)
    literal2 = Literal("2", INTEGER_SINGLE_TYPE)
    literal3 = Literal("3", INTEGER_SINGLE_TYPE)
    statement = Return()

    # Statements are not valid
    with pytest.raises(GenerationError) as excinfo:
        operation.addchild(statement)
    assert ("Item 'Return' can't be child 0 of 'BinaryOperation'. The valid "
            "format is: 'DataNode, DataNode'.") in str(excinfo.value)

    # First DataNodes is valid, but not subsequent ones
    operation.addchild(literal1)
    operation.addchild(literal2)
    with pytest.raises(GenerationError) as excinfo:
        operation.addchild(literal3)
    assert ("Item 'Literal' can't be child 2 of 'BinaryOperation'. The valid "
            "format is: 'DataNode, DataNode'.") in str(excinfo.value)


# Test UnaryOperation class
def test_unaryoperation_initialization():
    ''' Check the initialization method of the UnaryOperation class works
    as expected.'''

    with pytest.raises(TypeError) as err:
        _ = UnaryOperation("not an operator")
    assert "UnaryOperation operator argument must be of type " \
           "UnaryOperation.Operator but found" in str(err.value)
    uop = UnaryOperation(UnaryOperation.Operator.MINUS)
    assert uop._operator is UnaryOperation.Operator.MINUS


@pytest.mark.parametrize("operator_name", ['MINUS', 'PLUS', 'NOT'])
def test_unaryoperation_operator(operator_name):
    '''Test that the operator property returns the unaryoperator in the
    unaryoperation.

    '''
    operator = getattr(UnaryOperation.Operator, operator_name)
    unary_operation = UnaryOperation(operator)
    assert unary_operation.operator == operator


def test_unaryoperation_node_str():
    ''' Check the view method of the UnaryOperation class.'''
    ref1 = Reference(DataSymbol("a", REAL_SINGLE_TYPE))
    unary_operation = UnaryOperation.create(UnaryOperation.Operator.MINUS,
                                            ref1)
    coloredtext = colored("UnaryOperation", UnaryOperation._colour)
    assert coloredtext+"[operator:'MINUS']" in unary_operation.node_str()


def test_unaryoperation_can_be_printed():
    '''Test that a UnaryOperation instance can always be printed (i.e. is
    initialised fully)'''
    unary_operation = UnaryOperation(UnaryOperation.Operator.MINUS)
    assert "UnaryOperation[operator:'MINUS']" in str(unary_operation)
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    unary_operation.addchild(op1)
    # Check the node children are also printed
    assert ("Literal[value:'1', Scalar<INTEGER, SINGLE>]"
            in str(unary_operation))


def test_unaryoperation_create():
    '''Test that the create method in the UnaryOperation class correctly
    creates a UnaryOperation instance.

    '''
    child = Reference(DataSymbol("tmp", REAL_SINGLE_TYPE))
    oper = UnaryOperation.Operator.MINUS
    unaryoperation = UnaryOperation.create(oper, child)
    check_links(unaryoperation, [child])
    result = FortranWriter().unaryoperation_node(unaryoperation)
    assert result == "-tmp"


def test_unaryoperation_create_invalid4():
    '''Test that the create method in a UnaryOperation class raises the
    expected exception if the provided argument is a tuple with the
    wrong number of arguments.

    '''
    # oper not a UnaryOperator.Operator.
    with pytest.raises(GenerationError) as excinfo:
        _ = UnaryOperation.create(
            "invalid",
            Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)))
    assert ("operator argument in create method of UnaryOperation class "
            "should be a PSyIR UnaryOperation Operator but found 'str'."
            in str(excinfo.value))


def test_unaryoperation_children_validation():
    '''Test that children added to unaryOperation are validated.
    UnaryOperations accept just 1 DataNode as child.

    '''
    operation = UnaryOperation(UnaryOperation.Operator.MINUS)
    literal1 = Literal("1", INTEGER_SINGLE_TYPE)
    literal2 = Literal("2", INTEGER_SINGLE_TYPE)
    statement = Return()

    # Statements are not valid
    with pytest.raises(GenerationError) as excinfo:
        operation.addchild(statement)
    assert ("Item 'Return' can't be child 0 of 'UnaryOperation'. The valid "
            "format is: 'DataNode'.") in str(excinfo.value)

    # First DataNodes is valid, but not subsequent ones
    operation.addchild(literal1)
    with pytest.raises(GenerationError) as excinfo:
        operation.addchild(literal2)
    assert ("Item 'Literal' can't be child 1 of 'UnaryOperation'. The valid "
            "format is: 'DataNode'.") in str(excinfo.value)


def test_operations_can_be_copied():
    ''' Test that an operation can be copied. '''

    operands = [Reference(DataSymbol("tmp1", REAL_SINGLE_TYPE)),
                Reference(DataSymbol("tmp2", REAL_SINGLE_TYPE))]
    operation = BinaryOperation.create(BinaryOperation.Operator.ADD, *operands)

    operation1 = operation.copy()
    assert isinstance(operation1, BinaryOperation)
    assert operation1 is not operation
    assert operation1.operator is BinaryOperation.Operator.ADD
    assert operation1.children[0].symbol.name == "tmp1"
    assert operation1.children[0] is not operands[0]
    assert operation1.children[0].parent is operation1
    assert operation1.children[1].symbol.name == "tmp2"
    assert operation1.children[1] is not operands[1]
    assert operation1.children[1].parent is operation1
    assert len(operation1.children) == 2
    assert len(operation.children) == 2

    # Modifying the new operation does not affect the original
    operation1._operator = BinaryOperation.Operator.MUL
    operation1.children.pop()
    assert len(operation1.children) == 1
    assert len(operation.children) == 2
    assert operation1.operator is BinaryOperation.Operator.MUL
    assert operation.operator is BinaryOperation.Operator.ADD


def test_operation_equality():
    ''' Test the __eq__ method of Operation'''
    tmp1 = DataSymbol("tmp1", REAL_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", REAL_SINGLE_TYPE)
    lhs = Reference(tmp1)
    rhs = Reference(tmp2)
    oper = BinaryOperation.Operator.ADD
    binaryoperation1 = BinaryOperation.create(oper, lhs, rhs)

    oper = BinaryOperation.Operator.ADD
    binaryoperation2 = BinaryOperation.create(oper, lhs.copy(), rhs.copy())

    assert binaryoperation1 == binaryoperation2

    # change the operator
    binaryoperation2._operator = BinaryOperation.Operator.SUB
    assert binaryoperation1 != binaryoperation2
