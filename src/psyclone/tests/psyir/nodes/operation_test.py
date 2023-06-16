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

from psyclone.core import VariablesAccessInfo
from psyclone.psyir.nodes import UnaryOperation, BinaryOperation, \
    NaryOperation, Literal, Reference, Return
from psyclone.psyir.symbols import DataSymbol, INTEGER_SINGLE_TYPE, \
    REAL_SINGLE_TYPE
from psyclone.errors import GenerationError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import check_links
from psyclone.psyir.nodes import Assignment, colored


# Test Operation class. These are mostly covered by the subclass tests.

def test_operation_named_arg_str():
    '''Check the output of str from the Operation class when there is a
    mixture of positional and named arguments. We use a
    BinaryOperation example to exercise this method.

    '''
    lhs = Reference(DataSymbol("tmp1", REAL_SINGLE_TYPE))
    rhs = Reference(DataSymbol("tmp2", REAL_SINGLE_TYPE))
    oper = BinaryOperation.Operator.DOT_PRODUCT
    binaryoperation = BinaryOperation.create(oper, lhs, ("named_arg", rhs))
    assert "named_arg=Reference[name:'tmp2']" in str(binaryoperation)


def test_operation_appendnamedarg():
    '''Test the append_named_arg method in the Operation class. Check
    it raises the expected exceptions if arguments are invalid and
    that it works as expected when the input is valid. We use the
    NaryOperation node to perform the tests.

    '''
    nary_operation = NaryOperation(NaryOperation.Operator.MAX)
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    op2 = Literal("2", INTEGER_SINGLE_TYPE)
    op3 = Literal("3", INTEGER_SINGLE_TYPE)
    # name arg wrong type
    with pytest.raises(TypeError) as info:
        nary_operation.append_named_arg(1, op1)
    assert ("A name should be a string or None, but found int."
            in str(info.value))
    # name arg invalid
    with pytest.raises(ValueError) as info:
        nary_operation.append_named_arg("_", op2)
    assert "Invalid name '_' found." in str(info.value)
    # name arg already used
    nary_operation.append_named_arg("name1", op1)
    with pytest.raises(ValueError) as info:
        nary_operation.append_named_arg("name1", op2)
    assert ("The value of the name argument (name1) in 'append_named_arg' in "
            "the 'Operator' node is already used for a named argument."
            in str(info.value))
    # ok
    nary_operation.append_named_arg("name2", op2)
    nary_operation.append_named_arg(None, op3)
    assert nary_operation.children == [op1, op2, op3]
    assert nary_operation.argument_names == ["name1", "name2", None]
    # too many args
    binary_operation = BinaryOperation.create(
        BinaryOperation.Operator.DOT_PRODUCT, op1.copy(), op2.copy())
    with pytest.raises(GenerationError) as info:
        binary_operation.append_named_arg(None, op3.copy())
    assert ("Item 'Literal' can't be child 2 of 'BinaryOperation'. The valid "
            "format is: 'DataNode, DataNode'." in str(info.value))


def test_operation_insertnamedarg():
    '''Test the insert_named_arg method in the Operation class. Check
    it raises the expected exceptions if arguments are invalid and
    that it works as expected when the input is valid. We use the
    NaryOperation node to perform the tests.

    '''
    nary_operation = NaryOperation(NaryOperation.Operator.MAX)
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    op2 = Literal("2", INTEGER_SINGLE_TYPE)
    op3 = Literal("3", INTEGER_SINGLE_TYPE)
    # name arg wrong type
    with pytest.raises(TypeError) as info:
        nary_operation.insert_named_arg(1, op1, 0)
    assert ("A name should be a string or None, but found int."
            in str(info.value))
    # name arg invalid
    with pytest.raises(ValueError) as info:
        nary_operation.append_named_arg(" a", op2)
    assert "Invalid name ' a' found." in str(info.value)
    # name arg already used
    nary_operation.insert_named_arg("name1", op1, 0)
    with pytest.raises(ValueError) as info:
        nary_operation.insert_named_arg("name1", op2, 0)
    assert ("The value of the name argument (name1) in 'insert_named_arg' in "
            "the 'Operator' node is already used for a named argument."
            in str(info.value))
    # invalid index type
    with pytest.raises(TypeError) as info:
        nary_operation.insert_named_arg("name2", op2, "hello")
    assert ("The 'index' argument in 'insert_named_arg' in the 'Operator' "
            "node should be an int but found str." in str(info.value))
    # ok
    assert nary_operation.children == [op1]
    assert nary_operation.argument_names == ["name1"]
    nary_operation.insert_named_arg("name2", op2, 0)
    assert nary_operation.children == [op2, op1]
    assert nary_operation.argument_names == ["name2", "name1"]
    nary_operation.insert_named_arg(None, op3, 0)
    assert nary_operation.children == [op3, op2, op1]
    assert nary_operation.argument_names == [None, "name2", "name1"]
    # invalid index value
    binary_operation = BinaryOperation.create(
        BinaryOperation.Operator.DOT_PRODUCT, op1.copy(), op2.copy())
    with pytest.raises(GenerationError) as info:
        binary_operation.insert_named_arg("name2", op2.copy(), 2)
    assert ("Item 'Literal' can't be child 2 of 'BinaryOperation'. The valid "
            "format is: 'DataNode, DataNode'." in str(info.value))


def test_operation_replacenamedarg():
    '''Test the replace_named_arg method in the Operation class. Check
    it raises the expected exceptions if arguments are invalid and
    that it works as expected when the input is valid. We use the
    BinaryOperation node to perform the tests.

    '''
    binary_operation = BinaryOperation(BinaryOperation.Operator.DOT_PRODUCT)
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    op2 = Literal("2", INTEGER_SINGLE_TYPE)
    op3 = Literal("3", INTEGER_SINGLE_TYPE)
    binary_operation.append_named_arg("name1", op1)
    binary_operation.append_named_arg("name2", op2)

    # name arg wrong type
    with pytest.raises(TypeError) as info:
        binary_operation.replace_named_arg(1, op3)
    assert ("The 'name' argument in 'replace_named_arg' in the 'Operation' "
            "node should be a string, but found int."
            in str(info.value))
    # name arg is not found
    with pytest.raises(ValueError) as info:
        binary_operation.replace_named_arg("new_name", op3)
    assert ("The value of the existing_name argument (new_name) in "
            "'replace_named_arg' in the 'Operation' node was not found in the "
            "existing arguments." in str(info.value))
    # ok
    assert binary_operation.children == [op1, op2]
    assert binary_operation.argument_names == ["name1", "name2"]
    assert binary_operation._argument_names[0][0] == id(op1)
    assert binary_operation._argument_names[1][0] == id(op2)
    binary_operation.replace_named_arg("name1", op3)
    assert binary_operation.children == [op3, op2]
    assert binary_operation.argument_names == ["name1", "name2"]
    assert binary_operation._argument_names[0][0] == id(op3)
    assert binary_operation._argument_names[1][0] == id(op2)


def test_operation_argumentnames_after_removearg():
    '''Test the argument_names property makes things consistent if a child
    argument is removed. This is used transparently by the class to
    keep things consistent. We use the BinaryOperation node to perform
    the tests.

    '''
    binary_operation = BinaryOperation(BinaryOperation.Operator.DOT_PRODUCT)
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    op2 = Literal("1", INTEGER_SINGLE_TYPE)
    binary_operation.append_named_arg("name1", op1)
    binary_operation.append_named_arg("name2", op2)
    assert len(binary_operation.children) == 2
    assert len(binary_operation._argument_names) == 2
    assert binary_operation.argument_names == ["name1", "name2"]
    binary_operation.children.pop(0)
    assert len(binary_operation.children) == 1
    assert len(binary_operation._argument_names) == 2
    # argument_names property makes _argument_names list consistent.
    assert binary_operation.argument_names == ["name2"]
    assert len(binary_operation._argument_names) == 1


def test_operation_argumentnames_after_addarg():
    '''Test the argument_names property makes things consistent if a child
    argument is added. This is used transparently by the class to
    keep things consistent. We use the NaryOperation node to perform
    the tests (as it allows an arbitrary number of arguments.

    '''
    nary_operation = NaryOperation(NaryOperation.Operator.MAX)
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    op2 = Literal("1", INTEGER_SINGLE_TYPE)
    op3 = Literal("1", INTEGER_SINGLE_TYPE)
    nary_operation.append_named_arg("name1", op1)
    nary_operation.append_named_arg("name2", op2)
    assert len(nary_operation.children) == 2
    assert len(nary_operation._argument_names) == 2
    assert nary_operation.argument_names == ["name1", "name2"]
    nary_operation.children.append(op3)
    assert len(nary_operation.children) == 3
    assert len(nary_operation._argument_names) == 2
    # argument_names property makes _argument_names list consistent.
    assert nary_operation.argument_names == ["name1", "name2", None]
    assert len(nary_operation._argument_names) == 3


def test_operation_argumentnames_after_replacearg():
    '''Test the argument_names property makes things consistent if a child
    argument is replaced with another. This is used transparently by
    the class to keep things consistent. We use the BinaryOperation
    node to perform the tests.

    '''
    binary_operation = BinaryOperation(BinaryOperation.Operator.DOT_PRODUCT)
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    op2 = Literal("1", INTEGER_SINGLE_TYPE)
    op3 = Literal("1", INTEGER_SINGLE_TYPE)
    binary_operation.append_named_arg("name1", op1)
    binary_operation.append_named_arg("name2", op2)
    assert len(binary_operation.children) == 2
    assert len(binary_operation._argument_names) == 2
    assert binary_operation.argument_names == ["name1", "name2"]
    binary_operation.children[0] = op3
    assert len(binary_operation.children) == 2
    assert len(binary_operation._argument_names) == 2
    # argument_names property makes _argument_names list consistent.
    assert binary_operation.argument_names == [None, "name2"]
    assert len(binary_operation._argument_names) == 2


def test_operation_argumentnames_after_reorderearg():
    '''Test the argument_names property makes things consistent if child
    arguments are re-order. This is used transparently by the class to
    keep things consistent. We use the BinaryOperation node to perform
    the tests.

    '''
    binary_operation = BinaryOperation(BinaryOperation.Operator.DOT_PRODUCT)
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    op2 = Literal("1", INTEGER_SINGLE_TYPE)
    binary_operation.append_named_arg("name1", op1)
    binary_operation.append_named_arg("name2", op2)
    assert len(binary_operation.children) == 2
    assert len(binary_operation._argument_names) == 2
    assert binary_operation.argument_names == ["name1", "name2"]
    tmp0 = binary_operation.children[0]
    tmp1 = binary_operation.children[1]
    tmp0.detach()
    tmp1.detach()
    binary_operation.children.extend([tmp1, tmp0])
    assert len(binary_operation.children) == 2
    assert len(binary_operation._argument_names) == 2
    # argument_names property makes _argument_names list consistent.
    assert binary_operation.argument_names == ["name2", "name1"]
    assert len(binary_operation._argument_names) == 2


def test_operation_reconcile_add():
    '''Test that the reconcile method behaves as expected. Use an
    NaryOperation example where we add a new arg.

    '''
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    op2 = Literal("1", INTEGER_SINGLE_TYPE)
    op3 = Literal("1", INTEGER_SINGLE_TYPE)
    oper = NaryOperation.create(
        NaryOperation.Operator.MAX, [("name1", op1), ("name2", op2)])
    # consistent
    assert len(oper._argument_names) == 2
    assert oper._argument_names[0] == (id(oper.children[0]), "name1")
    assert oper._argument_names[1] == (id(oper.children[1]), "name2")
    oper.children.append(op3)
    # inconsistent
    assert len(oper._argument_names) == 2
    assert oper._argument_names[0] == (id(oper.children[0]), "name1")
    assert oper._argument_names[1] == (id(oper.children[1]), "name2")
    oper._reconcile()
    # consistent
    assert len(oper._argument_names) == 3
    assert oper._argument_names[0] == (id(oper.children[0]), "name1")
    assert oper._argument_names[1] == (id(oper.children[1]), "name2")
    assert oper._argument_names[2] == (id(oper.children[2]), None)


def test_operation_reconcile_reorder():
    '''Test that the reconcile method behaves as expected. Use a
    BinaryOperation example where we reorder the arguments.

    '''
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    op2 = Literal("2", INTEGER_SINGLE_TYPE)
    oper = BinaryOperation.create(
        BinaryOperation.Operator.DOT_PRODUCT, ("name1", op1), ("name2", op2))
    # consistent
    assert len(oper._argument_names) == 2
    assert oper._argument_names[0] == (id(oper.children[0]), "name1")
    assert oper._argument_names[1] == (id(oper.children[1]), "name2")
    oper.children = [op2.detach(), op1.detach()]
    # inconsistent
    assert len(oper._argument_names) == 2
    assert oper._argument_names[0] != (id(oper.children[0]), "name1")
    assert oper._argument_names[1] != (id(oper.children[1]), "name2")
    oper._reconcile()
    # consistent
    assert len(oper._argument_names) == 2
    assert oper._argument_names[0] == (id(oper.children[0]), "name2")
    assert oper._argument_names[1] == (id(oper.children[1]), "name1")


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


def test_binaryoperation_named_create():
    '''Test that the create method in the BinaryOperation class correctly
    creates a BinaryOperation instance when one or more of the
    arguments is a named argument.

    '''
    lhs = Reference(DataSymbol("tmp1", REAL_SINGLE_TYPE))
    rhs = Reference(DataSymbol("tmp2", REAL_SINGLE_TYPE))
    oper = BinaryOperation.Operator.DOT_PRODUCT
    binaryoperation = BinaryOperation.create(oper, lhs, ("dim", rhs))
    check_links(binaryoperation, [lhs, rhs])
    result = FortranWriter().binaryoperation_node(binaryoperation)
    assert result == "DOT_PRODUCT(tmp1, dim=tmp2)"
    binaryoperation = BinaryOperation.create(
        oper, ("dummy", lhs.detach()), ("dim", rhs.detach()))
    check_links(binaryoperation, [lhs, rhs])
    result = FortranWriter().binaryoperation_node(binaryoperation)
    assert result == "DOT_PRODUCT(dummy=tmp1, dim=tmp2)"


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

    # rhs is an invalid tuple (too many elements)
    oper = BinaryOperation.Operator.DOT_PRODUCT
    with pytest.raises(GenerationError) as excinfo:
        _ = BinaryOperation.create(oper, ref1, (1, 2, 3))
    assert ("If the rhs argument in create method of BinaryOperation class "
            "is a tuple, it's length should be 2, but it is 3."
            in str(excinfo.value))

    # rhs is an invalid tuple (1st element not str)
    oper = BinaryOperation.Operator.DOT_PRODUCT
    with pytest.raises(GenerationError) as excinfo:
        _ = BinaryOperation.create(oper, ref1, (1, 2))
    assert ("If the rhs argument in create method of BinaryOperation class "
            "is a tuple, its first argument should be a str, but found "
            "int." in str(excinfo.value))

    # rhs has an invalid name (1st element invalid value)
    oper = BinaryOperation.Operator.DOT_PRODUCT
    with pytest.raises(ValueError) as info:
        _ = BinaryOperation.create(oper, ref1.copy(), ("_", 2))
    assert "Invalid name '_' found." in str(info.value)


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


def test_binaryoperation_is_elemental():
    '''Test that the is_elemental method properly returns if an operation is
    elemental in each BinaryOperation.

    '''
    # MATMUL, SIZE, LBOUND, UBOUND and DOT_PRODUCT are not
    # elemental
    not_elemental = [
        BinaryOperation.Operator.SIZE,
        BinaryOperation.Operator.MATMUL,
        BinaryOperation.Operator.LBOUND,
        BinaryOperation.Operator.UBOUND,
        BinaryOperation.Operator.DOT_PRODUCT
    ]

    for binary_operator in BinaryOperation.Operator:
        operation = BinaryOperation(binary_operator)
        if binary_operator in not_elemental:
            assert operation.is_elemental is False
        else:
            assert operation.is_elemental is True


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
    oper = UnaryOperation.Operator.SIN
    unaryoperation = UnaryOperation.create(oper, child)
    check_links(unaryoperation, [child])
    result = FortranWriter().unaryoperation_node(unaryoperation)
    assert result == "SIN(tmp)"


def test_unaryoperation_named_create():
    '''Test that the create method in the UnaryOperation class correctly
    creates a UnaryOperation instance when there is a named argument.

    '''
    child = Reference(DataSymbol("tmp", REAL_SINGLE_TYPE))
    oper = UnaryOperation.Operator.SIN
    unaryoperation = UnaryOperation.create(oper, ("name", child))
    assert unaryoperation.argument_names == ["name"]
    check_links(unaryoperation, [child])
    result = FortranWriter().unaryoperation_node(unaryoperation)
    assert result == "SIN(name=tmp)"


def test_unaryoperation_create_invalid1():
    '''Test that the create method in a UnaryOperation class raises the
    expected exception if the provided argument is a tuple that does
    not have 2 elements.

    '''
    # oper not a UnaryOperator.Operator.
    oper = UnaryOperation.Operator.SIN
    with pytest.raises(GenerationError) as excinfo:
        _ = UnaryOperation.create(
            oper, (1, 2, 3))
    assert ("If the argument in the create method of UnaryOperation class is "
            "a tuple, it's length should be 2, but it is 3."
            in str(excinfo.value))


def test_unaryoperation_create_invalid2():
    '''Test that the create method in a UnaryOperation class raises the
    expected exception if the provided argument is a tuple and the
    first element of the tuple is not a string.

    '''
    # oper not a UnaryOperator.Operator.
    oper = UnaryOperation.Operator.SIN
    with pytest.raises(GenerationError) as excinfo:
        _ = UnaryOperation.create(
            oper, (1, 2))
    assert ("If the argument in the create method of UnaryOperation class "
            "is a tuple, its first argument should be a str, but found int."
            in str(excinfo.value))


def test_unaryoperation_create_invalid3():
    '''Test that the create method in a UnaryOperation class raises the
    expected exception a named argument is provided with an invalid name.

    '''
    oper = UnaryOperation.Operator.SIN
    with pytest.raises(ValueError) as info:
        _ = UnaryOperation.create(oper, ("1", 2))
    assert "Invalid name '1' found." in str(info.value)


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


def test_unaryoperation_is_elemental():
    '''Test that the is_elemental method properly returns if an operation is
    elemental in each UnaryOperation.

    '''
    # All unary operators are elemental
    for unary_operator in UnaryOperation.Operator:
        operation = UnaryOperation(unary_operator)
        assert operation.is_elemental is True


# Test NaryOperation class
def test_naryoperation_node_str():
    ''' Check the node_str method of the Nary Operation class.'''
    nary_operation = NaryOperation(NaryOperation.Operator.MAX)
    nary_operation.addchild(Literal("1", INTEGER_SINGLE_TYPE))
    nary_operation.addchild(Literal("1", INTEGER_SINGLE_TYPE))
    nary_operation.addchild(Literal("1", INTEGER_SINGLE_TYPE))

    coloredtext = colored("NaryOperation", NaryOperation._colour)
    assert coloredtext+"[operator:'MAX']" in nary_operation.node_str()


def test_naryoperation_can_be_printed():
    '''Test that an Nary Operation instance can always be printed (i.e. is
    initialised fully)'''
    nary_operation = NaryOperation(NaryOperation.Operator.MAX)
    assert "NaryOperation[operator:'MAX']" in str(nary_operation)
    nary_operation.addchild(Literal("1", INTEGER_SINGLE_TYPE))
    nary_operation.addchild(Literal("2", INTEGER_SINGLE_TYPE))
    nary_operation.addchild(Literal("3", INTEGER_SINGLE_TYPE))
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


def test_naryoperation_named_create():
    '''Test that the create method in the NaryOperation class correctly
    creates a NaryOperation instance when one of the arguments is a
    named argument.

    '''
    children = [Reference(DataSymbol("tmp1", REAL_SINGLE_TYPE)),
                Reference(DataSymbol("tmp2", REAL_SINGLE_TYPE)),
                Reference(DataSymbol("tmp3", REAL_SINGLE_TYPE))]
    oper = NaryOperation.Operator.MAX
    naryoperation = NaryOperation.create(
        oper, [children[0], children[1], ("name", children[2])])
    check_links(naryoperation, children)
    result = FortranWriter().naryoperation_node(naryoperation)
    assert result == "MAX(tmp1, tmp2, name=tmp3)"


def test_naryoperation_create_invalid():
    '''Test that the create method in an NaryOperation class raises the
    expected exception if the provided input is invalid.

    '''
    # oper not an NaryOperation.Operator
    with pytest.raises(GenerationError) as excinfo:
        _ = NaryOperation.create("invalid", [])
    assert ("operator argument in create method of NaryOperation class should "
            "be a PSyIR NaryOperation Operator but found 'str'."
            in str(excinfo.value))

    oper = NaryOperation.Operator.MAX

    # children not a list
    with pytest.raises(GenerationError) as excinfo:
        _ = NaryOperation.create(oper, "invalid")
    assert ("operands argument in create method of NaryOperation class should "
            "be a list but found 'str'." in str(excinfo.value))

    ref1 = Reference(DataSymbol("tmp1", REAL_SINGLE_TYPE))
    ref2 = Reference(DataSymbol("tmp2", REAL_SINGLE_TYPE))

    # rhs is an invalid tuple (too many elements)
    with pytest.raises(GenerationError) as excinfo:
        _ = NaryOperation.create(oper, [ref1, ref2, (1, 2, 3)])
    assert ("If an element of the operands argument in create method of "
            "NaryOperation class is a tuple, it's length should be 2, "
            "but found 3." in str(excinfo.value))

    # rhs is an invalid tuple (1st element not str)
    with pytest.raises(GenerationError) as excinfo:
        _ = NaryOperation.create(oper, [ref1.copy(), ref2.copy(), (1, 2)])
    assert ("If an element of the operands argument in create method of "
            "NaryOperation class is a tuple, its first argument should "
            "be a str, but found int." in str(excinfo.value))


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


def test_naryoperation_is_elemental():
    '''Test that the is_elemental method properly returns if an operation is
    elemental in each NaryOperation.

    '''
    # All nary operations are elemental
    for nary_operator in NaryOperation.Operator:
        operation = NaryOperation(nary_operator)
        assert operation.is_elemental is True


def test_operations_can_be_copied():
    ''' Test that an operation can be copied. '''

    operands = [Reference(DataSymbol("tmp1", REAL_SINGLE_TYPE)),
                Reference(DataSymbol("tmp2", REAL_SINGLE_TYPE)),
                Reference(DataSymbol("tmp3", REAL_SINGLE_TYPE))]
    operation = NaryOperation.create(NaryOperation.Operator.MAX, operands)

    operation1 = operation.copy()
    assert isinstance(operation1, NaryOperation)
    assert operation1 is not operation
    assert operation1.operator is NaryOperation.Operator.MAX
    assert operation1.children[0].symbol.name == "tmp1"
    assert operation1.children[0] is not operands[0]
    assert operation1.children[0].parent is operation1
    assert operation1.children[1].symbol.name == "tmp2"
    assert operation1.children[1] is not operands[1]
    assert operation1.children[1].parent is operation1
    assert operation1.children[2].symbol.name == "tmp3"
    assert operation1.children[2] is not operands[2]
    assert operation1.children[2].parent is operation1
    assert len(operation1.children) == 3
    assert len(operation.children) == 3

    # Modifying the new operation does not affect the original
    operation1._operator = NaryOperation.Operator.MIN
    operation1.children.pop()
    assert len(operation1.children) == 2
    assert len(operation.children) == 3
    assert operation1.operator is NaryOperation.Operator.MIN
    assert operation.operator is NaryOperation.Operator.MAX


def test_copy():
    '''Test that the copy() method behaves as expected when there are
    named arguments.

    '''
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    op2 = Literal("2", INTEGER_SINGLE_TYPE)
    oper = BinaryOperation.create(
        BinaryOperation.Operator.DOT_PRODUCT, ("name1", op1), ("name2", op2))
    # consistent operation
    oper_copy = oper.copy()
    assert oper._argument_names[0] == (id(oper.children[0]), "name1")
    assert oper._argument_names[1] == (id(oper.children[1]), "name2")
    assert oper_copy._argument_names[0] == (id(oper_copy.children[0]), "name1")
    assert oper_copy._argument_names[1] == (id(oper_copy.children[1]), "name2")
    assert oper._argument_names != oper_copy._argument_names

    oper.children = [op2.detach(), op1.detach()]
    assert oper._argument_names[0] != (id(oper.children[0]), "name2")
    assert oper._argument_names[1] != (id(oper.children[1]), "name1")
    # inconsistent operation
    oper_copy = oper.copy()
    assert oper._argument_names[0] == (id(oper.children[0]), "name2")
    assert oper._argument_names[1] == (id(oper.children[1]), "name1")
    assert oper_copy._argument_names[0] == (id(oper_copy.children[0]), "name2")
    assert oper_copy._argument_names[1] == (id(oper_copy.children[1]), "name1")
    assert oper._argument_names != oper_copy._argument_names


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

    # Check with arguments names
    binaryoperation3 = BinaryOperation.create(
        oper, ("name1", lhs.copy()), rhs.copy())
    binaryoperation4 = BinaryOperation.create(
        oper, ("name1", lhs.copy()), rhs.copy())
    assert binaryoperation3 == binaryoperation4

    # Check with argument name and no argument name
    assert binaryoperation3 != binaryoperation1

    # Check with different argument names
    binaryoperation5 = BinaryOperation.create(
        oper, ("new_name", lhs.copy()), rhs.copy())
    assert binaryoperation3 != binaryoperation5


@pytest.mark.parametrize("operator", ["lbound", "ubound", "size"])
def test_reference_accesses_bounds(operator, fortran_reader):
    '''Test that the reference_accesses method behaves as expected when
    the reference is the first argument to either the lbound or ubound
    intrinsic as that is simply looking up the array bounds (therefore
    var_access_info should be empty) and when the reference is the
    second argument of either the lbound or ubound intrinsic (in which
    case the access should be a read).

    '''
    code = f'''module test
        contains
        subroutine tmp()
          real, dimension(:,:), allocatable:: a, b
          integer :: n
          n = {operator}(a, b(1,1))
        end subroutine tmp
        end module test'''
    psyir = fortran_reader.psyir_from_source(code)
    schedule = psyir.walk(Assignment)[0]

    # By default, the access to 'a' should not be reported as read,
    # but the access to b must be reported:
    vai = VariablesAccessInfo(schedule)
    assert str(vai) == "b: READ, n: WRITE"

    # When explicitly requested, the access to 'a' should be reported:
    vai = VariablesAccessInfo(schedule,
                              options={"COLLECT-ARRAY-SHAPE-READS": True})
    assert str(vai) == "a: READ, b: READ, n: WRITE"
