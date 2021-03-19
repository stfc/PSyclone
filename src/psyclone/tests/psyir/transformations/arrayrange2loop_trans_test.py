# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford and S. Siso, STFC Daresbury Lab

'''Module containing tests for the ArrayRange2LoopTrans
transformation.'''

from __future__ import absolute_import

import pytest

from psyclone.psyir.nodes import Literal, BinaryOperation, Reference, \
    Range, ArrayReference, Assignment, Node, DataNode, KernelSchedule
from psyclone.psyGen import Transformation
from psyclone.psyir.symbols import SymbolTable, DataSymbol, ArrayType, \
    INTEGER_TYPE, REAL_TYPE
from psyclone.psyir.transformations import ArrayRange2LoopTrans, \
    TransformationError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import Compile


def create_range(array_symbol, dim):
    '''Utility routine that creates and returns a Range Node that
    specifies the full range of the supplied dimension (dim) in the
    array (array_symbol). This is done using the LBOUND and UBOUND
    intrinsics.

    :param array_symbol: the array of interest.
    :type array_symbol: :py:class:`psyclone.psyir.symbol.DataSymbol`
    :param int dim: the dimension of interest in the array.

    :returns: a range node specifying the full range of the supplied \
        array dimension.
    :rtype: :py:class:`psyclone.psyir.nodes.Range`

    '''
    int_dim = Literal(str(dim), INTEGER_TYPE)
    lbound = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(array_symbol), int_dim)
    ubound = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND,
        Reference(array_symbol), int_dim.copy())
    return Range.create(lbound, ubound)


def create_stepped_range(symbol):
    '''Utility routine that creates and returns a Range Node that
    specifies a range from "2" to "symbol" step "2".

    :param symbol: the symbol representing the upper bound.
    :type symbol: :py:class:`psyclone.psyir.symbol.Symbol`

    :returns: a range node specifying a range from 2 to "symbol" with \
        a step of 2 for the supplied array dimension.
    :rtype: :py:class:`psyclone.psyir.nodes.Range`

    '''
    lbound = Literal("2", INTEGER_TYPE)
    ubound = Reference(symbol)
    step = Literal("2", INTEGER_TYPE)
    return Range.create(lbound, ubound, step)


def create_literal(_):
    '''Utility routine that creates and returns a literal node. We choose
    a real with value 0.0 but it could be any literal type. Takes an
    empty argument to be consistent with other create calls.

    :returns: a real literal node with value 0.0.
    :rtype: :py:class:`psyclone.psyir.nodes.Literal`

    '''
    return Literal("0.0", REAL_TYPE)


def create_array_x(symbol_table):
    '''Utility routine that creates and returns an array reference to a
    one-dimensional array "x". The array reference accesses all of the
    elements in the array dimension using a range node. In Fortran
    array notation this is "x(:)".

    :param symbol_table: the symbol table to which we add the array \
        symbol.
    :type symbol_table: :py:class:`psyclone.psyir.symbol.SymbolTable`

    :returns: an array reference that accesses all elements of the \
        array "x".
    :rtype: :py:class:`psyclone.psyir.nodes.ArrayReference`

    '''
    array_symbol = DataSymbol("x", ArrayType(REAL_TYPE, [10]))
    symbol_table.add(array_symbol)
    return ArrayReference.create(array_symbol, [create_range(array_symbol, 1)])


def create_array_y(symbol_table):
    '''Utility routine that creates and returns an array reference to a
    two-dimensional array "y". The array reference accesses all
    elements in the 2nd dimension of the array using a range node and
    the n'th element of the 1st dimension. In Fortran array notation
    this is "y(n,:)".

    :param symbol_table: the symbol table to which we add the array \
        symbol and access the symbol "n".
    :type symbol_table: :py:class:`psyclone.psyir.symbol.SymbolTable`

    :returns: an array reference that accesses all elements of the \
        2nd dimension of array "y" and the n'th element of its 1st \
        dimension.
    :rtype: :py:class:`psyclone.psyir.nodes.ArrayReference`

    '''
    array_symbol = DataSymbol("y", ArrayType(REAL_TYPE, [10, 10]))
    symbol_table.add(array_symbol)
    return ArrayReference.create(array_symbol,
                                 [Reference(symbol_table.lookup("n")),
                                  create_range(array_symbol, 2)])


def create_array_y_2d_slice(symbol_table):
    '''Utility routine that creates and returns an array reference to a 2
    dimensional array "y". The array reference accesses all elements
    in the 1st and 2nd dimensions of the array using range nodes. In
    Fortran array notation this is "y(:,:)".

    :param symbol_table: the symbol table to which we add the array \
        symbol.
    :type symbol_table: :py:class:`psyclone.psyir.symbol.SymbolTable`

    :returns: an array reference that accesses all elements of the 1st \
        and 2nd dimensions of the "y" array.
    :rtype: :py:class:`psyclone.psyir.nodes.ArrayReference`

    '''
    array_symbol = DataSymbol("y2", ArrayType(REAL_TYPE, [20, 10]))
    symbol_table.add(array_symbol)
    return ArrayReference.create(array_symbol, [create_range(array_symbol, 1),
                                                create_range(array_symbol, 2)])


def create_array_z(symbol_table):
    '''Utility routine that creates and returns an array reference to a 3
    dimensional array "z". The array reference accesses all elements
    in the arrays 1st and 3rd dimensions using range nodes and
    accesses element "n" in its second dimension. In Fortran array
    notation this is "z(:,n,:)".

    :param symbol_table: the symbol table to which we add the array \
        symbol and access the symbol "n"
    :type symbol_table: :py:class:`psyclone.psyir.symbol.SymbolTable`

    :returns: an array reference that accesses all elements of the 1st \
        and 3rd dimensions of array "z" and the n'th element of its \
        second dimension.
    :rtype: :py:class:`psyclone.psyir.nodes.ArrayReference`

    '''
    array_symbol = DataSymbol("z", ArrayType(REAL_TYPE, [20, 10, 10]))
    symbol_table.add(array_symbol)
    return ArrayReference.create(array_symbol,
                                 [create_range(array_symbol, 1),
                                  Reference(symbol_table.lookup("n")),
                                  create_range(array_symbol, 3)])


def create_array_y_slice_subset(symbol_table):
    '''Utility routine that creates and returns an array reference to a 2
    dimensional array "y". The array reference accesses elements 2 to
    "n" step 2 in the arrays 2nd dimension using a range node and the
    n'th element of the 1st dimension. In Fortran array notation this
    is "y(n,2:n:2)".

    :param symbol_table: the symbol table to which we add the array \
        symbol and access the symbol "n".
    :type symbol_table: :py:class:`psyclone.psyir.symbol.SymbolTable`

    :returns: an array reference that accesses elements 2 to "n" step \
        2 in the array "y"'s 2nd dimension and the n'th element of its \
        1st dimension.
    :rtype: :py:class:`psyclone.psyir.nodes.ArrayReference`

    '''
    array_symbol = DataSymbol("y3", ArrayType(REAL_TYPE, [10, 10]))
    symbol_table.add(array_symbol)
    symbol_n = symbol_table.lookup("n")
    return ArrayReference.create(array_symbol,
                                 [Reference(symbol_n),
                                  create_stepped_range(symbol_n)])


def create_expr(symbol_table):
    '''Utility routine that creates and returns an expression containing a
    number of array references containing range nodes. The expression
    looks like the following (using Fortran array notation):

    x(2:n:2)*z(1,2:n:2)+a(1)

    :param symbol_table: the symbol table to which we add the array \
        symbols.
    :type symbol_table: :py:class:`psyclone.psyir.symbol.SymbolTable`

    :returns: an expression containing 3 array references, 2 of which \
        contain ranges.
    :rtype: :py:class:`psyclone.psyir.nodes.BinaryOperation`

    '''
    array_symbol = DataSymbol("x", ArrayType(REAL_TYPE, [10]))
    symbol_table.add(array_symbol)
    symbol_n = symbol_table.lookup("n")
    array_x = ArrayReference.create(array_symbol,
                                    [create_stepped_range(symbol_n)])
    array_symbol = DataSymbol("z", ArrayType(REAL_TYPE, [10, 10]))
    symbol_table.add(array_symbol)
    array_z = ArrayReference.create(array_symbol,
                                    [Literal("1", INTEGER_TYPE),
                                     create_stepped_range(symbol_n)])
    array_symbol = DataSymbol("a", ArrayType(REAL_TYPE, [10]))
    array_a = ArrayReference.create(array_symbol, [Literal("1", INTEGER_TYPE)])
    symbol_table.add(array_symbol)
    mult = BinaryOperation.create(
        BinaryOperation.Operator.MUL, array_x, array_z)
    add = BinaryOperation.create(BinaryOperation.Operator.ADD, mult, array_a)
    return add


def test_transform():
    '''Check that it is possible to create an instance of
    ArrayRange2LoopTrans and that it is a Transformation.

    '''
    assert ArrayRange2LoopTrans()
    assert isinstance(ArrayRange2LoopTrans(), Transformation)


def test_string_compare():
    '''Check that the string_compare utility function in
    ArrayRange2LoopTrans works as expected.

    '''
    with pytest.raises(TypeError) as info:
        ArrayRange2LoopTrans.string_compare(None, None)
    assert (
        "The first argument to the string_compare method should be a Node "
        "but found 'NoneType'." in str(info.value))

    with pytest.raises(TypeError) as info:
        ArrayRange2LoopTrans.string_compare(Node(), None)
    assert (
        "The second argument to the string_compare method should be a Node "
        "but found 'NoneType'." in str(info.value))

    node1 = Literal("1.0", REAL_TYPE)
    node2 = BinaryOperation.create(BinaryOperation.Operator.MUL, node1, node1)
    node3 = BinaryOperation.create(BinaryOperation.Operator.MAX, node2, node2)
    assert ArrayRange2LoopTrans.string_compare(node3, node3) is True
    assert ArrayRange2LoopTrans.string_compare(node3, node2) is False


def test_same_range():
    '''Test that the same_range utility function behaves in the expected
    way.

    '''
    with pytest.raises(TypeError) as info:
        ArrayRange2LoopTrans.same_range(None, None, None, None)
    assert ("The first argument to the same_range() method should be an "
            "ArrayReference but found 'NoneType'." in str(info.value))

    array_type = ArrayType(REAL_TYPE, [10])
    array_value = ArrayReference.create(
        DataSymbol("dummy", array_type), [DataNode("x")])
    array_range = ArrayReference.create(
        DataSymbol("dummy", array_type), [Range()])

    with pytest.raises(TypeError) as info:
        ArrayRange2LoopTrans.same_range(array_value, None, None, None)
    assert ("The second argument to the same_range() method should be an "
            "int but found 'NoneType'." in str(info.value))

    with pytest.raises(TypeError) as info:
        ArrayRange2LoopTrans.same_range(array_value, 1, None, None)
    assert ("The third argument to the same_range() method should be an "
            "ArrayReference but found 'NoneType'." in str(info.value))

    with pytest.raises(TypeError) as info:
        ArrayRange2LoopTrans.same_range(array_value, 1, array_value, None)
    assert ("The fourth argument to the same_range() method should be an "
            "int but found 'NoneType'." in str(info.value))

    with pytest.raises(IndexError) as info:
        ArrayRange2LoopTrans.same_range(array_value, 1, array_value, 2)
    assert ("The value of the second argument to the same_range() method "
            "'1' should be less than the number of dimensions '1' in the "
            "associated array 'array1'." in str(info.value))

    with pytest.raises(IndexError) as info:
        ArrayRange2LoopTrans.same_range(array_value, 0, array_value, 2)
    assert ("The value of the fourth argument to the same_range() method "
            "'2' should be less than the number of dimensions '1' in the "
            "associated array 'array2'." in str(info.value))

    with pytest.raises(TypeError) as info:
        ArrayRange2LoopTrans.same_range(array_value, 0, array_value, 0)
    assert ("The child of the first array argument at the specified index (0) "
            "should be a Range node, but found 'DataNode'" in str(info.value))

    with pytest.raises(TypeError) as info:
        ArrayRange2LoopTrans.same_range(array_range, 0, array_value, 0)
    assert ("The child of the second array argument at the specified index "
            "(0) should be a Range node, but found 'DataNode'"
            in str(info.value))

    # lower bounds both use lbound, upper bounds both use ubound and
    # step is the same so everything matches.
    array_x = create_array_x(SymbolTable())
    array_x_2 = create_array_x(SymbolTable())
    assert ArrayRange2LoopTrans.same_range(array_x, 0, array_x_2, 0) is True

    # steps are different (calls string_compare)
    tmp = array_x_2.children[0].step
    array_x_2.children[0].step = Literal("2", INTEGER_TYPE)
    assert ArrayRange2LoopTrans.same_range(array_x, 0, array_x_2, 0) is False

    # Put step value back to what it was in case it affects the ubound
    # and lbound tests
    array_x_2.children[0].step = tmp

    # one of upper bounds uses ubound, other does not
    tmp1 = array_x_2.children[0].stop
    array_x_2.children[0].stop = Literal("2", INTEGER_TYPE)
    assert ArrayRange2LoopTrans.same_range(array_x, 0, array_x_2, 0) is False

    # neither use upper bound and are different (calls string_compare)
    tmp2 = array_x.children[0].stop
    array_x.children[0].stop = Literal("1", INTEGER_TYPE)
    assert ArrayRange2LoopTrans.same_range(array_x, 0, array_x_2, 0) is False

    # Put upper bounds back to what they were in case they affect the
    # lbound tests
    array_x_2.children[0].stop = tmp1
    array_x.children[0].stop = tmp2

    # one of lower bounds uses lbound, other does not
    array_x_2.children[0].start = Literal("1", INTEGER_TYPE)
    assert ArrayRange2LoopTrans.same_range(array_x, 0, array_x_2, 0) is False

    # neither use lower bound and are different (calls string_compare)
    array_x.children[0].start = Literal("2", INTEGER_TYPE)
    assert ArrayRange2LoopTrans.same_range(array_x, 0, array_x_2, 0) is False


# The parametrised tests are 1: x(:)=0.0, 2: x(:)=y(n,:), 3:
# y(n,:)=x(:), 4: y2(:,:)=z(:,n,:) and 5:
# y3(n,2:n:2)=x(2:n:2)*z(1,2:n:2)+a(1)
@pytest.mark.parametrize("lhs_create,rhs_create,expected",
                         [(create_array_x, create_literal,
                           "  do idx = LBOUND(x, 1), UBOUND(x, 1), 1\n"
                           "    x(idx) = 0.0\n"),
                          (create_array_x, create_array_y,
                           "  do idx = LBOUND(x, 1), UBOUND(x, 1), 1\n"
                           "    x(idx) = y(n,idx)\n"),
                          (create_array_y, create_array_x,
                           "  do idx = LBOUND(y, 2), UBOUND(y, 2), 1\n"
                           "    y(n,idx) = x(idx)\n"),
                          (create_array_y_2d_slice, create_array_z,
                           "  do idx = LBOUND(y2, 2), UBOUND(y2, 2), 1\n"
                           "    y2(:,idx) = z(:,n,idx)\n"),
                          (create_array_y_slice_subset, create_expr,
                           "  do idx = 2, n, 2\n"
                           "    y3(n,idx) = x(idx) * z(1,idx) + a(1)")])
def test_transform_apply(lhs_create, rhs_create, expected, tmpdir):
    '''Check that the PSyIR is transformed as expected for various types
    of ranges in an array. The resultant Fortran code is used to
    confirm the transformation has worked correctly.

    '''
    trans = ArrayRange2LoopTrans()
    symbol_table = SymbolTable()
    symbol = DataSymbol("n", INTEGER_TYPE)
    symbol_table.add(symbol)
    lhs = lhs_create(symbol_table)
    rhs = rhs_create(symbol_table)
    assignment = Assignment.create(lhs, rhs)
    routine = KernelSchedule.create("work", symbol_table,
                                    [assignment])
    trans.apply(assignment)
    writer = FortranWriter()
    result = writer(routine)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_transform_multi_apply(tmpdir):
    '''Check that the ArrayRange2Loop transformation can be used to create
    nested loops by calling it multiple times when an array has
    multiple dimensions that use a range.

    '''
    trans = ArrayRange2LoopTrans()

    symbol_table = SymbolTable()
    symbol = DataSymbol("n", INTEGER_TYPE)
    symbol_table.add(symbol)
    lhs = create_array_y_2d_slice(symbol_table)
    rhs = create_array_z(symbol_table)
    assignment = Assignment.create(lhs, rhs)
    routine = KernelSchedule.create("work", symbol_table,
                                    [assignment])
    trans.apply(assignment)
    trans.apply(assignment)
    expected = (
        "  do idx = LBOUND(y2, 2), UBOUND(y2, 2), 1\n"
        "    do idx_1 = LBOUND(y2, 1), UBOUND(y2, 1), 1\n"
        "      y2(idx_1,idx) = z(idx_1,n,idx)\n"
        "    enddo\n"
        "  enddo\n")
    writer = FortranWriter()
    result = writer(routine)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_transform_apply_insert(tmpdir):
    '''Check that the PSyIR is transformed as expected when there are
    multiple statements in the PSyIR. The resultant Fortran code is used to
    confirm the transformation has worked correctly.

    '''
    trans = ArrayRange2LoopTrans()

    symbol_table = SymbolTable()
    symbol = DataSymbol("n", INTEGER_TYPE)
    symbol_table.add(symbol)
    # Create the first assignment. In Fortran notation: x(:) = y(n,:)
    lhs = create_array_x(symbol_table)
    rhs = create_array_y(symbol_table)
    assignment1 = Assignment.create(lhs, rhs)
    # Create the second assignment. In Fortran notation: y2(:,:) = z(:,n,:)
    lhs = create_array_y_2d_slice(symbol_table)
    rhs = create_array_z(symbol_table)
    assignment2 = Assignment.create(lhs, rhs)
    routine = KernelSchedule.create("work", symbol_table,
                                    [assignment1, assignment2])
    trans.apply(assignment1)
    trans.apply(assignment2)
    writer = FortranWriter()
    expected = (
        "  do idx = LBOUND(x, 1), UBOUND(x, 1), 1\n"
        "    x(idx) = y(n,idx)\n"
        "  enddo\n"
        "  do idx_1 = LBOUND(y2, 2), UBOUND(y2, 2), 1\n"
        "    y2(:,idx_1) = z(:,n,idx_1)\n"
        "  enddo\n")
    result = writer(routine)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_apply_calls_validate():
    '''Check that the apply() method calls the validate method.'''
    trans = ArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert("Error in ArrayRange2LoopTrans transformation. The supplied node "
           "argument should be a PSyIR Assignment, but found 'NoneType'."
           in str(info.value))


def test_str():
    '''Test that the str of an instance of the ArrayRange2LoopTrans class
    returns the expected value.

    '''
    assert (str(ArrayRange2LoopTrans()) == "Convert a PSyIR assignment to an "
            "array Range into a PSyIR Loop.")


def test_name():
    '''Check that the name property of the ArrayRange2LoopTrans class
    returns the expected value.

    '''
    assert ArrayRange2LoopTrans().name == "ArrayRange2LoopTrans"


def test_validate():
    '''Test that the validate method in the ArrayRange2LoopTrans class
    raises the expected exceptions.

    '''
    trans = ArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(Node())
    assert (
        "Error in ArrayRange2LoopTrans transformation. The supplied node "
        "argument should be a PSyIR Assignment, but found 'Node'."
        in str(info.value))

    with pytest.raises(TransformationError) as info:
        trans.validate(Assignment.create(DataNode(), DataNode()))
    assert (
        "Error in ArrayRange2LoopTrans transformation. The lhs of the "
        "supplied Assignment node should be a PSyIR ArrayReference, but found "
        "'DataNode'." in str(info.value))

    array_symbol = DataSymbol("x", ArrayType(INTEGER_TYPE, [10, 10]))
    one = Literal("1", INTEGER_TYPE)
    array_assignment = ArrayReference.create(array_symbol, [one, one])
    with pytest.raises(TransformationError) as info:
        trans.validate(Assignment.create(array_assignment, DataNode()))
    assert (
        "Error in ArrayRange2LoopTrans transformation. The lhs of the "
        "supplied Assignment node should be a PSyIR ArrayReference with at "
        "least one "
        "of its dimensions being a Range, but found None in "
        "'ArrayReference[name:'x']\\nLiteral[value:'1', "
        "Scalar<INTEGER, UNDEFINED>]\\nLiteral[value:'1', Scalar<INTEGER, "
        "UNDEFINED>]\\n'." in str(info.value))

    array_x = create_array_x(SymbolTable())
    assignment = Assignment.create(
        create_array_x(SymbolTable()), array_x)
    trans.validate(assignment)

    array_x.children[0].step = Literal("2", INTEGER_TYPE)
    with pytest.raises(TransformationError) as info:
        trans.validate(assignment)
    assert (
        "The ArrayRange2LoopTrans transformation only supports ranges that "
        "are known to be the same as each other but array access 'x' "
        "dimension 0 and 'x' dimension 0 are either different or can't be "
        "determined in the assignment 'Assignment[]\\n"
        "ArrayReference[name:'x']\\nRange[]\\n"
        "ArrayReference[name:'x']\\nRange[]\\n'."
        in str(info.value))


def test_validate_intrinsic():
    '''Check that the validate method returns an exception if the rhs of
    the assignment contains an operator that only returns an array
    i.e. can't be performed elementwise. At the moment MATMUL is the
    only operator of this type.

    '''
    symbol_table = SymbolTable()
    array_x = create_array_x(symbol_table)
    array_y_2 = create_array_y_2d_slice(symbol_table)
    matmul = BinaryOperation.create(BinaryOperation.Operator.MATMUL,
                                    array_y_2, array_x)
    reference = ArrayReference.create(
        symbol_table.lookup("x"), [create_range(symbol_table.lookup("x"), 1)])
    assignment = Assignment.create(reference, matmul)

    trans = ArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(assignment)
    assert (
        "Error in ArrayRange2LoopTrans transformation. The rhs of the "
        "supplied Assignment node 'BinaryOperation[operator:'MATMUL']\\n"
        "ArrayReference[name:'y2']\\nRange[]\\nRange[]\\n\\n"
        "ArrayReference[name:'x']\\nRange[]\\n' contains the "
        "MATMUL operator which can't be performed elementwise." in
        str(info.value))
