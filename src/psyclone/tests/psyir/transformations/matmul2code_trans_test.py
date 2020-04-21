# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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

'''Module containing tests for the matmul2code transformation.'''

from __future__ import absolute_import
import pytest
from psyclone.psyir.transformations import Matmul2CodeTrans, \
    TransformationError
from psyclone.psyir.nodes import BinaryOperation, Literal, Array, Assignment, \
    Reference, Range
from psyclone.psyGen import KernelSchedule
from psyclone.psyir.symbols import DataType, DataSymbol, SymbolTable
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import Compile

def create_matmul():
    '''Utility function that creates a valid matmul node for use with
    subsequent tests.

    '''
    symbol_table = SymbolTable()
    one = Literal("1", DataType.INTEGER)
    two = Literal("2", DataType.INTEGER)
    index = DataSymbol("idx", DataType.INTEGER)
    symbol_table.add(index)
    mat_symbol = DataSymbol("x", DataType.REAL, shape=[10, 10, 10])
    symbol_table.add(mat_symbol)
    lbound1 = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, Reference(mat_symbol), one)
    ubound1 = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, Reference(mat_symbol), one)
    my_mat_range1 = Range.create(lbound1, ubound1, one)
    lbound2 = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, Reference(mat_symbol), two)
    ubound2 = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, Reference(mat_symbol), two)
    my_mat_range2 = Range.create(lbound2, ubound2, one)
    matrix = Array.create(mat_symbol, [my_mat_range1, my_mat_range2,
                                       Reference(index)])
    vec_symbol = DataSymbol("y", DataType.REAL, shape=[10, 10])
    symbol_table.add(vec_symbol)
    lbound = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, Reference(vec_symbol), one)
    ubound = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, Reference(vec_symbol), one)
    my_vec_range = Range.create(lbound, ubound, one)
    vector = Array.create(vec_symbol, [my_vec_range, Reference(index)])
    matmul = BinaryOperation.create(
        BinaryOperation.Operator.MATMUL, matrix, vector)
    assign = Assignment.create(vector, matmul)
    KernelSchedule.create("my_kern", symbol_table, [assign])
    return matmul


def test_initialise():
    '''Check that the str and name methods work as expected.

    '''
    trans = Matmul2CodeTrans()
    assert (str(trans) == "Convert the PSyIR MATMUL intrinsic to equivalent "
            "PSyIR code.")
    assert trans.name == "Matmul2CodeTrans"


def test_validate1():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is the wrong type.

    '''
    trans = Matmul2CodeTrans()
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(None)
    assert ("The supplied node should be a BinaryOperation but found "
            "'NoneType'." in str(excinfo.value))


def test_validate2():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a binary operation but not a
    MATMUL.

    '''
    trans = Matmul2CodeTrans()
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(BinaryOperation.create(
            BinaryOperation.Operator.ADD, Literal("1.0", DataType.REAL),
            Literal("1.0", DataType.REAL)))
    assert ("Transformation Error: The supplied node should be a MATMUL "
            "BinaryOperation but found 'Operator.ADD'." in str(excinfo.value))


def test_validate3():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL binary operation but
    it is not the only operation on the RHS of an assignment.

    '''
    trans = Matmul2CodeTrans()
    array = Array.create(DataSymbol("x", DataType.REAL, shape=[10]),
                         [Literal("10", DataType.INTEGER)])
    matmul = BinaryOperation.create(
        BinaryOperation.Operator.MATMUL, array, array)
    _ = BinaryOperation.create(BinaryOperation.Operator.MUL, matmul,
                               Literal("1.0", DataType.REAL))
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: Matmul2CodeTrans only supports the "
            "transformation of a MATMUL operation when it is the sole "
            "operation on the rhs of an assignment." in str(excinfo.value))


def test_validate4():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL binary operation but
    either or both arguments are not references.

    '''
    trans = Matmul2CodeTrans()
    array = Array.create(DataSymbol("x", DataType.REAL, shape=[10]),
                         [Literal("10", DataType.INTEGER)])
    mult = BinaryOperation.create(
        BinaryOperation.Operator.MUL, array, array)
    matmul = BinaryOperation.create(
        BinaryOperation.Operator.MATMUL, mult, mult)
    _ = Assignment.create(array, matmul)
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Expected children of a MATMUL BinaryOperation to be references, "
            "but found 'BinaryOperation', 'BinaryOperation'."
            in str(excinfo.value))


def test_validate5():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL binary operation but
    either or both of its arguments are references to datasymbols that
    are not arrays.

    '''
    trans = Matmul2CodeTrans()
    scalar = Reference(DataSymbol("x", DataType.REAL))
    matmul = BinaryOperation.create(
        BinaryOperation.Operator.MATMUL, scalar, scalar)
    _ = Assignment.create(scalar, matmul)
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: Expected children of a MATMUL "
            "BinaryOperation to be references to arrays, but found "
            "'DataSymbol', 'DataSymbol'." in str(excinfo.value))


def test_validate6():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL binary operation but
    its first (matrix) argument has less than 2 dimensions.

    '''
    trans = Matmul2CodeTrans()
    array = Reference(DataSymbol("x", DataType.REAL, shape=[10]))
    matmul = BinaryOperation.create(
        BinaryOperation.Operator.MATMUL, array, array)
    _ = Assignment.create(array, matmul)
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: Expected 1st child of a MATMUL "
            "BinaryOperation to be a matrix with at least 2 dimensions, "
            "but found '1'." in str(excinfo.value))


def test_validate7():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL binary operation but
    the first two dimensions of its first (matrix) argument are not
    full ranges.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    matrix = matmul.children[0]
    matrix.children[0] = Literal("1", DataType.INTEGER)
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: To use matmul2code_trans on matmul, "
            "indices 0 and 1 of the 1st (matrix) argument 'x' must be "
            "full ranges." in str(excinfo.value))


def test_validate8():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL binary operation but
    the third (or higher) dimension of the first (matrix) argument is
    indexed via a range.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    matrix = matmul.children[0]
    my_range = matrix.children[0]
    matrix.children[2] = my_range
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: To use matmul2code_trans on matmul, "
            "only the first two indices of the 1st (matrix) argument are "
            "permitted to be Ranges but found Range at index 2."
            in str(excinfo.value))


def test_validate9():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL binary operation but
    the first dimension of its second (vector) argument is not a full
    range.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    vector = matmul.children[1]
    vector.children[0] = Literal("1", DataType.INTEGER)
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: To use matmul2code_trans on matmul, "
            "index 0 of the 2nd (vector) argument 'x' must be a full range."
            in str(excinfo.value))


def test_validate10():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL binary operation but
    the second (or higher) dimension of the second (vector) argument is
    indexed via a range.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    vector = matmul.children[1]
    my_range = vector.children[0]
    vector.children[1] = my_range
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: To use matmul2code_trans on matmul, "
            "only the first index of the 2nd (vector) argument is "
            "permitted to be a Range but found Range at index 1."
            in str(excinfo.value))


def test_validate11():
    '''Check that the Matmul2Code validate method returns without any
    exceptions when the supplied node is a MATMUL binary operation
    that obeys the required rules and constraints.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    trans.validate(matmul)


def test_apply1(tmpdir):
    '''Test that the matmul2code apply method produces the expected
    PSyIR. We use the Fortran backend to help provide the test for
    correctness. This example includes extra indices for the vector
    and matrix arrays with these indices being references.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    trans.apply(matmul)
    writer = FortranWriter()
    result = writer(matmul.root)
    assert (
        "subroutine my_kern()\n"
        "  integer :: idx\n"
        "  real, dimension(10,10,10) :: x\n"
        "  real, dimension(10,10) :: y\n"
        "  integer :: i\n"
        "  integer :: j\n"
        "\n"
        "  do i = 1, 10, 1\n"
        "    y(i,idx)=0.0\n"
        "    do j = 1, 10, 1\n"
        "      y(i,idx)=y(i,idx) + x(i,j,idx) * y(j,idx)\n"
        "    enddo\n"
        "  enddo\n"
        "\n"
        "end subroutine my_kern" in result)
    assert Compile(tmpdir).string_compiles(result)


def test_apply2(tmpdir):
    '''Test that the matmul2code apply method produces the expected
    PSyIR. We use the Fortran backend to help provide the test for
    correctness. This example includes extra indices for the vector
    and matrix arrays with additional indices being literals.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    matmul.children[0].children[2] = Literal("1", DataType.INTEGER)
    matmul.children[1].children[1] = Literal("2", DataType.INTEGER)
    trans.apply(matmul)
    writer = FortranWriter()
    result = writer(matmul.root)
    assert (
        "subroutine my_kern()\n"
        "  integer :: idx\n"
        "  real, dimension(10,10,10) :: x\n"
        "  real, dimension(10,10) :: y\n"
        "  integer :: i\n"
        "  integer :: j\n"
        "\n"
        "  do i = 1, 10, 1\n"
        "    y(i,2)=0.0\n"
        "    do j = 1, 10, 1\n"
        "      y(i,2)=y(i,2) + x(i,j,1) * y(j,2)\n"
        "    enddo\n"
        "  enddo\n"
        "\n"
        "end subroutine my_kern" in result)
    assert Compile(tmpdir).string_compiles(result)


def test_apply3(tmpdir):
    '''Test that the matmul2code apply method produces the expected
    PSyIR. We use the Fortran backend to help provide the test for
    correctness. This example includes the array and vector being
    passed with no index information.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    matrix = matmul.children[0]
    matrix_symbol = matrix.symbol
    matrix_symbol._shape = [10, 10]
    matmul.children[0] = Reference(matrix_symbol)
    rhs_vector = matmul.children[1]
    rhs_vector_symbol = rhs_vector.symbol
    rhs_vector_symbol._shape = [10]
    matmul.children[1] = Reference(rhs_vector_symbol)
    lhs_vector = matrix.parent.parent.lhs
    lhs_vector_symbol = lhs_vector.symbol
    lhs_vector_symbol._shape = [10]
    matrix.parent.parent.children[0] = Reference(lhs_vector_symbol)
    trans.apply(matmul)
    writer = FortranWriter()
    result = writer(matmul.root)
    assert (
        "subroutine my_kern()\n"
        "  integer :: idx\n"
        "  real, dimension(10,10) :: x\n"
        "  real, dimension(10) :: y\n"
        "  integer :: i\n"
        "  integer :: j\n"
        "\n"
        "  do i = 1, 10, 1\n"
        "    y(i)=0.0\n"
        "    do j = 1, 10, 1\n"
        "      y(i)=y(i) + x(i,j) * y(j)\n"
        "    enddo\n"
        "  enddo\n"
        "\n"
        "end subroutine my_kern" in result)
    assert Compile(tmpdir).string_compiles(result)
