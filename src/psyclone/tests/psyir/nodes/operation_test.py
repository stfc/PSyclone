# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2021, Science and Technology Facilities Council.
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

''' Performs pytest tests on the Operation PSyIR node and its sub-classes. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import UnaryOperation, BinaryOperation, \
    NaryOperation, Literal, Reference, Return
from psyclone.psyir.symbols import DataSymbol, INTEGER_SINGLE_TYPE, \
    REAL_SINGLE_TYPE
from psyclone.errors import GenerationError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import check_links


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
    from psyclone.psyir.nodes.node import colored
    binary_operation = BinaryOperation(BinaryOperation.Operator.ADD)
    op1 = Literal("1", INTEGER_SINGLE_TYPE, parent=binary_operation)
    op2 = Literal("1", INTEGER_SINGLE_TYPE, parent=binary_operation)
    binary_operation.addchild(op1)
    binary_operation.addchild(op2)
    coloredtext = colored("BinaryOperation", BinaryOperation._colour)
    assert coloredtext+"[operator:'ADD']" in binary_operation.node_str()


def test_binaryoperation_can_be_printed():
    '''Test that a Binary Operation instance can always be printed (i.e. is
    initialised fully)'''
    binary_operation = BinaryOperation(BinaryOperation.Operator.ADD)
    assert "BinaryOperation[operator:'ADD']" in str(binary_operation)
    op1 = Literal("1", INTEGER_SINGLE_TYPE, parent=binary_operation)
    op2 = Literal("2", INTEGER_SINGLE_TYPE, parent=binary_operation)
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
    assert ("oper argument in create method of BinaryOperation class should "
            "be a PSyIR BinaryOperation Operator but found 'str'."
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


def test_unaryoperation_operator():
    '''Test that the operator property returns the unaryoperator in the
    unaryoperation.

    '''
    unary_operation = UnaryOperation(UnaryOperation.Operator.MINUS)
    assert unary_operation.operator == UnaryOperation.Operator.MINUS


def test_unaryoperation_node_str():
    ''' Check the view method of the UnaryOperation class.'''
    from psyclone.psyir.nodes.node import colored
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
    op1 = Literal("1", INTEGER_SINGLE_TYPE, parent=unary_operation)
    unary_operation.addchild(op1)
    # Check the node children are also printed
    assert ("Literal[value:'1', Scalar<INTEGER, SINGLE>]"
            in str(unary_operation))


def test_unaryoperation_create():
    '''Test that the create method in the UnaryOperation class correctly
    creates a UnaryOperation instance.

    '''
    child = Reference(DataSymbol("tmp", REAL_SINGLE_TYPE))
    oper = UnaryOperation.Operator.SIN
    unaryoperation = UnaryOperation.create(oper, child)
    check_links(unaryoperation, [child])
    result = FortranWriter().unaryoperation_node(unaryoperation)
    assert result == "SIN(tmp)"


def test_unaryoperation_create_invalid():
    '''Test that the create method in a UnaryOperation class raises the
    expected exception if the provided input is invalid.

    '''
    # oper not a UnaryOperator.Operator.
    with pytest.raises(GenerationError) as excinfo:
        _ = UnaryOperation.create(
            "invalid",
            Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)))
    assert ("oper argument in create method of UnaryOperation class should "
            "be a PSyIR UnaryOperation Operator but found 'str'."
            in str(excinfo.value))


def test_unaryoperation_children_validation():
    '''Test that children added to unaryOperation are validated.
    UnaryOperations accept just 1 DataNode as child.

    '''
    operation = UnaryOperation(UnaryOperation.Operator.SIN)
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


def test_naryoperation_node_str():
    ''' Check the node_str method of the Nary Operation class.'''
    from psyclone.psyir.nodes.node import colored
    nary_operation = NaryOperation(NaryOperation.Operator.MAX)
    nary_operation.addchild(Literal("1", INTEGER_SINGLE_TYPE,
                                    parent=nary_operation))
    nary_operation.addchild(Literal("1", INTEGER_SINGLE_TYPE,
                                    parent=nary_operation))
    nary_operation.addchild(Literal("1", INTEGER_SINGLE_TYPE,
                                    parent=nary_operation))

    coloredtext = colored("NaryOperation", NaryOperation._colour)
    assert coloredtext+"[operator:'MAX']" in nary_operation.node_str()


def test_naryoperation_can_be_printed():
    '''Test that an Nary Operation instance can always be printed (i.e. is
    initialised fully)'''
    nary_operation = NaryOperation(NaryOperation.Operator.MAX)
    assert "NaryOperation[operator:'MAX']" in str(nary_operation)
    nary_operation.addchild(Literal("1", INTEGER_SINGLE_TYPE,
                                    parent=nary_operation))
    nary_operation.addchild(Literal("2", INTEGER_SINGLE_TYPE,
                                    parent=nary_operation))
    nary_operation.addchild(Literal("3", INTEGER_SINGLE_TYPE,
                                    parent=nary_operation))
    # Check the node children are also printed
    assert ("Literal[value:'1', Scalar<INTEGER, SINGLE>]\n"
            in str(nary_operation))
    assert ("Literal[value:'2', Scalar<INTEGER, SINGLE>]\n"
            in str(nary_operation))
    assert ("Literal[value:'3', Scalar<INTEGER, SINGLE>]"
            in str(nary_operation))


def test_naryoperation_create():
    '''Test that the create method in the NaryOperation class correctly
    creates an NaryOperation instance.

    '''
    children = [Reference(DataSymbol("tmp1", REAL_SINGLE_TYPE)),
                Reference(DataSymbol("tmp2", REAL_SINGLE_TYPE)),
                Reference(DataSymbol("tmp3", REAL_SINGLE_TYPE))]
    oper = NaryOperation.Operator.MAX
    naryoperation = NaryOperation.create(oper, children)
    check_links(naryoperation, children)
    result = FortranWriter().naryoperation_node(naryoperation)
    assert result == "MAX(tmp1, tmp2, tmp3)"


def test_naryoperation_create_invalid():
    '''Test that the create method in an NaryOperation class raises the
    expected exception if the provided input is invalid.

    '''
    # oper not an NaryOperation.Operator
    with pytest.raises(GenerationError) as excinfo:
        _ = NaryOperation.create("invalid", [])
    assert ("oper argument in create method of NaryOperation class should "
            "be a PSyIR NaryOperation Operator but found 'str'."
            in str(excinfo.value))

    # children not a list
    with pytest.raises(GenerationError) as excinfo:
        _ = NaryOperation.create(NaryOperation.Operator.SUM, "invalid")
    assert ("children argument in create method of NaryOperation class should "
            "be a list but found 'str'." in str(excinfo.value))


def test_naryoperation_children_validation():
    '''Test that children added to NaryOperation are validated. NaryOperations
    accepts DataNodes nodes as children.

    '''
    nary = NaryOperation(NaryOperation.Operator.MAX)
    literal1 = Literal("1", INTEGER_SINGLE_TYPE)
    literal2 = Literal("2", INTEGER_SINGLE_TYPE)
    literal3 = Literal("3", INTEGER_SINGLE_TYPE)
    statement = Return()

    # DataNodes are valid
    nary.addchild(literal1)
    nary.addchild(literal2)
    nary.addchild(literal3)

    # Statements are not valid
    with pytest.raises(GenerationError) as excinfo:
        nary.addchild(statement)
    assert ("Item 'Return' can't be child 3 of 'NaryOperation'. The valid "
            "format is: '[DataNode]+'.") in str(excinfo.value)
