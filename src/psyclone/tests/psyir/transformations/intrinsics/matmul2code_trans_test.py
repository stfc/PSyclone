# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Laboratory
#          T. Vockerodt, Met Office

'''Module containing tests for the matmul2code transformation.'''

import pytest
from psyclone.psyir.transformations import Matmul2CodeTrans, \
    TransformationError
from psyclone.psyir.transformations.intrinsics.matmul2code_trans import (
    _create_array_ref)
from psyclone.psyir.nodes import BinaryOperation, Literal, ArrayReference, \
    Assignment, Reference, Range, KernelSchedule, IntrinsicCall
from psyclone.psyir.symbols import (DataSymbol, SymbolTable, ArrayType,
                                    INTEGER_TYPE, REAL_TYPE)
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import Compile


def create_matmul():
    '''Utility function that creates a valid matmul node for use with
    subsequent tests.

    '''
    symbol_table = SymbolTable()
    one = Literal("1", INTEGER_TYPE)
    two = Literal("2", INTEGER_TYPE)
    index = DataSymbol("idx", INTEGER_TYPE, is_constant=True, initial_value=3)
    symbol_table.add(index)
    array_type = ArrayType(REAL_TYPE, [5, 10, 15])
    mat_symbol = DataSymbol("x", array_type)
    symbol_table.add(mat_symbol)
    lbound1 = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(mat_symbol), ("dim", one.copy())])
    ubound1 = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [Reference(mat_symbol), ("dim", one.copy())])
    my_mat_range1 = Range.create(lbound1, ubound1, one.copy())
    lbound2 = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(mat_symbol), ("dim", two.copy())])
    ubound2 = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [Reference(mat_symbol), ("dim", two.copy())])
    my_mat_range2 = Range.create(lbound2, ubound2, one.copy())
    matrix = ArrayReference.create(mat_symbol, [my_mat_range1, my_mat_range2,
                                                Reference(index)])
    array_type = ArrayType(REAL_TYPE, [10, 20, 10])
    vec_symbol = DataSymbol("y", array_type)
    symbol_table.add(vec_symbol)
    lbound = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(vec_symbol), ("dim", one.copy())])
    ubound = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [Reference(vec_symbol), ("dim", one.copy())])
    my_vec_range = Range.create(lbound, ubound, one.copy())
    vector = ArrayReference.create(vec_symbol, [my_vec_range,
                                                Reference(index), one.copy()])
    matmul = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.MATMUL, [matrix, vector])
    lhs_type = ArrayType(REAL_TYPE, [10])
    lhs_symbol = DataSymbol("result", lhs_type)
    symbol_table.add(lhs_symbol)
    lhs = Reference(lhs_symbol)
    assign = Assignment.create(lhs, matmul)
    KernelSchedule.create("my_kern", symbol_table, [assign])
    return matmul


def test_create_array_ref_1d():
    ''' Test that the _create_array_ref() utility works as expected for a
    1d array.

    '''
    array_type = ArrayType(REAL_TYPE, [10])
    array_symbol = DataSymbol("x", array_type)
    i_loop_sym = DataSymbol("i", INTEGER_TYPE)
    ref1 = _create_array_ref(array_symbol, [i_loop_sym], [], [0], [])
    assert isinstance(ref1, ArrayReference)
    assert ref1.symbol is array_symbol
    assert len(ref1.indices) == 1
    assert ref1.indices[0].symbol is i_loop_sym


def test_create_array_ref_trailing_indices():
    ''' Test that the _create_array_ref() utility works as expected for an
    array that has an additional dimension that is not being looped over.

    '''
    array_type = ArrayType(REAL_TYPE, [10, 5])
    array_symbol = DataSymbol("x", array_type)
    i_loop_sym = DataSymbol("i", INTEGER_TYPE)
    k_sym = DataSymbol("k", INTEGER_TYPE)
    k_ref = Reference(k_sym)
    ref1 = _create_array_ref(array_symbol, [i_loop_sym], [k_ref], [0], [1])
    assert isinstance(ref1, ArrayReference)
    assert ref1.symbol is array_symbol
    assert len(ref1.indices) == 2
    assert ref1.indices[0].symbol is i_loop_sym
    # The second index should be a *copy* of the expression we supplied.
    assert ref1.indices[1].symbol is k_sym
    assert ref1.indices[1] is not k_ref


def test_create_array_ref_2d():
    ''' Test that the _create_array_ref() utility works as expected for a
    2d array.

    '''
    array_type = ArrayType(REAL_TYPE, [10, 8])
    array_symbol = DataSymbol("x", array_type)
    i_loop_sym = DataSymbol("i", INTEGER_TYPE)
    j_loop_sym = DataSymbol("j", INTEGER_TYPE)
    ref2 = _create_array_ref(array_symbol,
                             [i_loop_sym, j_loop_sym],
                             [],
                             [0, 1],
                             [])
    assert isinstance(ref2, ArrayReference)
    assert ref2.symbol is array_symbol
    assert len(ref2.indices) == 2
    assert ref2.indices[0].symbol is i_loop_sym
    assert ref2.indices[1].symbol is j_loop_sym


def test_initialise():
    '''Check that the str and name methods work as expected.

    '''
    trans = Matmul2CodeTrans()
    assert (str(trans) == "Convert the PSyIR 'MATMUL' intrinsic to "
            "equivalent PSyIR code.")
    assert trans.name == "Matmul2CodeTrans"


def test_validate_node_wrong_type():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is the wrong type.

    '''
    trans = Matmul2CodeTrans()
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(None)
    assert ("Error in Matmul2CodeTrans transformation. The supplied node must "
            "be an 'IntrinsicCall', but found 'NoneType'."
            in str(excinfo.value))


def test_validate_node_not_matmul():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is an IntrinsicCall but not a MATMUL.

    '''
    trans = Matmul2CodeTrans()
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(IntrinsicCall.create(
            IntrinsicCall.Intrinsic.SUM, [Literal("1.0", REAL_TYPE)]))
    assert ("Transformation Error: Error in Matmul2CodeTrans transformation. "
            "The supplied IntrinsicCall must be a 'MATMUL' but "
            "found: 'SUM'." in str(excinfo.value))


def test_validate_no_assignment_ancestor():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL IntrinsicCall but
    doesn't have an assignment as an ancestor.

    '''
    trans = Matmul2CodeTrans()
    vector_type = ArrayType(REAL_TYPE, [10])
    array_type = ArrayType(REAL_TYPE, [10, 10])
    vector = Reference(DataSymbol("x", vector_type))
    array = Reference(DataSymbol("y", array_type))
    matmul = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.MATMUL, [array, vector])
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: Error in Matmul2CodeTrans transformation. "
            "This transformation requires the operator to be part of an "
            "assignment statement, but no such assignment was found."
            in str(excinfo.value))


def test_validate_not_solely_matmul():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL IntrinsicCall but
    it is not the only operation on the RHS of an assignment.

    '''
    trans = Matmul2CodeTrans()
    vector_type = ArrayType(REAL_TYPE, [10])
    array_type = ArrayType(REAL_TYPE, [10, 10])
    vector = Reference(DataSymbol("x", vector_type))
    array = Reference(DataSymbol("y", array_type))
    matmul = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.MATMUL, [array, vector])
    rhs = BinaryOperation.create(
        BinaryOperation.Operator.MUL, matmul, vector.copy())
    _ = Assignment.create(array.copy(), rhs)
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: Matmul2CodeTrans only supports the "
            "transformation of a MATMUL operation when it is the sole "
            "operation on the rhs of an assignment." in str(excinfo.value))


def test_validate_arg_not_ref():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL IntrinsicCall but
    either or both arguments are not references.

    '''
    trans = Matmul2CodeTrans()
    array_type = ArrayType(REAL_TYPE, [10])
    array = ArrayReference.create(DataSymbol("x", array_type),
                                  [Literal("10", INTEGER_TYPE)])
    mult = BinaryOperation.create(
        BinaryOperation.Operator.MUL, array.copy(), array.copy())
    matmul = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.MATMUL, [mult.copy(), mult.copy()])
    _ = Assignment.create(array, matmul)
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Expected result and operands of MATMUL IntrinsicCall to be "
            "references, but found: 'x(10) = MATMUL(x(10) * x(10), x(10) * "
            "x(10))\n'." in str(excinfo.value))


def test_validate_arg_not_arr():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL IntrinsicCall but
    either or both of its arguments are references to datasymbols that
    are not arrays.

    '''
    trans = Matmul2CodeTrans()
    scalar = Reference(DataSymbol("x", REAL_TYPE))
    matmul = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.MATMUL, [scalar, scalar.copy()])
    _ = Assignment.create(scalar.copy(), matmul)
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Expected result and operands of MATMUL IntrinsicCall to be "
            "references to arrays but found" in str(excinfo.value))


def test_validate_structure_accesses(fortran_reader):
    '''
    Check that the validate() method rejects the case where one or more
    arguments to the MATMUL are structure accesses.

    '''
    psyir = fortran_reader.psyir_from_source(
        "subroutine my_sub()\n"
        "  use my_mod, only: grid_type\n"
        "  type(grid_type) :: grid, grid_inv\n"
        "  real, dimension(5,5) :: result\n"
        "  result = matmul(grid%data, grid_inv%data)\n"
        "end subroutine my_sub\n")
    trans = Matmul2CodeTrans()
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as err:
        trans.apply(assign.rhs)
    assert ("Expected result and operands of MATMUL IntrinsicCall to be "
            "references to arrays but found" in str(err.value))


def test_validate_mat_too_few_dims():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL intrinsic but
    its first (matrix) argument has fewer than 2 dimensions.

    '''
    trans = Matmul2CodeTrans()
    array_type = ArrayType(REAL_TYPE, [10])
    array = Reference(DataSymbol("x", array_type))
    matmul = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.MATMUL, [array.copy(), array.copy()])
    _ = Assignment.create(array, matmul)
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: Expected 1st child of a MATMUL "
            "IntrinsicCall to be a matrix with at least 2 dimensions, "
            "but found '1'." in str(excinfo.value))


def test_validate_mat_too_many_dims():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL intrinsic but
    its first (matrix) argument is a reference to a matrix with
    greater than 2 dimensions.

    '''
    trans = Matmul2CodeTrans()
    array_type = ArrayType(REAL_TYPE, [10, 10, 10])
    array = Reference(DataSymbol("x", array_type))
    matmul = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.MATMUL, [array.copy(), array.copy()])
    _ = Assignment.create(array, matmul)
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: Expected 1st child of a MATMUL "
            "IntrinsicCall to have 2 dimensions, but found '3'."
            in str(excinfo.value))


def test_validate_vec_too_many_dims():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL intrinsic but its
    second argument is a reference to a matrix with more than 2 dimensions.

    '''
    trans = Matmul2CodeTrans()
    array_type = ArrayType(REAL_TYPE, [10, 10])
    array = Reference(DataSymbol("x", array_type))
    vector_type = ArrayType(REAL_TYPE, [10, 10, 10])
    vector = Reference(DataSymbol("y", vector_type))
    matmul = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.MATMUL, [array, vector])
    _ = Assignment.create(array.copy(), matmul)
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: Expected 2nd child of a MATMUL "
            "IntrinsicCall to have 1 or 2 dimensions, but found '3'."
            in str(excinfo.value))


def test_validate_mat_too_few_full_ranges():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL IntrinsicCall but
    less than two full ranges are specified in the first matrix.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    matrix = matmul.arguments[0]
    matrix.children[0] = Literal("1", INTEGER_TYPE)
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("To use Matmul2CodeTrans on matmul, 2 indices of the "
            "array 'x(1,:,idx)' must be full ranges "
            "but found 1." in str(excinfo.value))


def test_validate_vec_too_few_full_ranges():
    '''
    Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL IntrinsicCall but
    no full ranges are specified in the second (vector) argument.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    vector = matmul.arguments[1]
    my_index = vector.children[2].copy()
    vector.children[0] = my_index
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: To use Matmul2CodeTrans on matmul, "
            "either 1 or 2 indices of the array 'y(1,idx,1)' "
            "must be full ranges but found 0." in str(excinfo.value))


def test_validate_mat_non_full_range():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL IntrinsicCall but
    a dimension of the first (matrix) argument is indexed via a partial range.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    matrix = matmul.arguments[0]
    matrix.children[2] = Range.create(Literal("1", INTEGER_TYPE),
                                      Literal("10", INTEGER_TYPE))
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: To use Matmul2CodeTrans on matmul, "
            "each Range index of the array 'x(:,:,:10)' "
            "must be a full range but found "
            "non full range at position 2." in str(excinfo.value))


def test_validate_vec_non_full_range():
    '''Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL IntrinsicCall but
    a dimension of the second (vector) argument is indexed via a partial range.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    vector = matmul.arguments[1]
    vector.children[2] = Range.create(Literal("1", INTEGER_TYPE),
                                      Literal("5", INTEGER_TYPE))
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: To use Matmul2CodeTrans on matmul, "
            "each Range index of the array 'y(:,idx,:5)' "
            "must be a full range but found "
            "non full range at position 2." in str(excinfo.value))


def test_validate_mat_too_many_full_ranges():
    '''
    Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL IntrinsicCall but
    more than two full ranges are specified in the first (matrix) argument.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    matrix = matmul.arguments[0]
    matrix.children[2] = Range.create(Literal("1", INTEGER_TYPE),
                                      Literal("15", INTEGER_TYPE))
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: To use Matmul2CodeTrans on matmul, "
            "no more than two indices of the array "
            "'x(:,:,:)' must be full ranges "
            "but found 3." in str(excinfo.value))


def test_validate_vec_too_many_full_ranges():
    '''
    Check that the Matmul2Code validate method raises the expected
    exception when the supplied node is a MATMUL IntrinsicCall but
    more than two full ranges are specified in the second (vector) argument.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    vector = matmul.arguments[1]
    vector.children[1] = Range.create(Literal("1", INTEGER_TYPE),
                                      Literal("20", INTEGER_TYPE))
    vector.children[2] = Range.create(Literal("1", INTEGER_TYPE),
                                      Literal("10", INTEGER_TYPE))
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(matmul)
    assert ("Transformation Error: To use Matmul2CodeTrans on matmul, "
            "no more than two indices of the array "
            "'y(:,:,:)' must be full ranges but found 3."
            in str(excinfo.value))


def test_validate_good_input():
    '''Check that the Matmul2Code validate method returns without any
    exceptions when the supplied node is a MATMUL IntrinsicCall
    that obeys the required rules and constraints.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    trans.validate(matmul)


def test_validate_matmat_with_slices_on_rhs(fortran_reader):
    '''
    Check that the validate method refuses matrix-matrix operations with
    array slides in its lhs.

    '''
    psyir = fortran_reader.psyir_from_source(
        "subroutine my_sub()\n"
        "  real, dimension(2,6) :: jac\n"
        "  real, dimension(6,3) :: jac_inv\n"
        "  real, dimension(10,10) :: result\n"
        "  result(2:4,2:5) = matmul(jac(:,:), jac_inv(:,:))\n"
        "end subroutine my_sub\n")
    trans = Matmul2CodeTrans()
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(assign.rhs)
    assert ("Transformation Error: To use Matmul2CodeTrans on matmul, each "
            "Range index of the array 'result(2:4,2:5)' must be "
            "a full range but found non full range "
            "at position 0." in str(excinfo.value))


def test_validate_res_too_many_full_ranges(fortran_reader):
    '''
    Check that the validate method refuses matrix-matrix operations with
    too many full ranges in its lhs.

    '''
    psyir = fortran_reader.psyir_from_source(
        "subroutine my_sub()\n"
        "  real, dimension(2,6) :: jac\n"
        "  real, dimension(6,3) :: jac_inv\n"
        "  real, dimension(10,10,10) :: result\n"
        "  result(:,:,:) = matmul(jac(:,:), jac_inv(:,:))\n"
        "end subroutine my_sub\n")
    trans = Matmul2CodeTrans()
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(assign.rhs)
    assert ("Transformation Error: To use Matmul2CodeTrans on matmul, "
            "no more than two indices of the array 'result(:,:,:)' "
            "must be full ranges but found 3." in str(excinfo.value))


def test_validate_res_too_few_full_ranges(fortran_reader):
    '''
    Check that the validate method refuses matrix-matrix operations with
    too few full ranges in its lhs.

    '''
    psyir = fortran_reader.psyir_from_source(
        "subroutine my_sub()\n"
        "  real, dimension(2,6) :: jac\n"
        "  real, dimension(6,3) :: jac_inv\n"
        "  real, dimension(10,10) :: result\n"
        "  result(10,10) = matmul(jac(:,:), jac_inv(:,:))\n"
        "end subroutine my_sub\n")
    trans = Matmul2CodeTrans()
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(assign.rhs)
    assert ("Transformation Error: To use Matmul2CodeTrans on matmul, "
            "either 1 or 2 indices of the array 'result(10,10)' "
            "must be full ranges but found 0." in str(excinfo.value))


def test_validate_matmat_with_same_mem(fortran_reader):
    '''
    Check that the validate method refuses cases where one of the operands
    is also the lhs of the matrix multiplication.

    '''
    psyir = fortran_reader.psyir_from_source(
        "subroutine my_sub()\n"
        "  real, dimension(2,2) :: jac\n"
        "  real, dimension(2,2) :: jac_inv\n"
        "  jac = matmul(jac(:,:), jac_inv(:,:))\n"
        "end subroutine my_sub\n")
    trans = Matmul2CodeTrans()
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(assign.rhs)
    assert ("Transformation Error: 'jac' is the result location and one of the"
            " MATMUL operators. This is not supported." in str(excinfo.value))

    # In the version below we can not guarantee whether the memory is the same
    psyir = fortran_reader.psyir_from_source(
        "subroutine my_sub()\n"
        "  real, dimension(2,2) :: jac\n"
        "  real, dimension(2,2), pointer :: jac_inv\n"
        "  real, dimension(2,2), pointer :: result\n"
        "  result = matmul(jac(:,:), jac_inv(:,:))\n"
        "end subroutine my_sub\n")
    trans = Matmul2CodeTrans()
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(assign.rhs)
    assert ("Transformation Error: Must have full type information for result "
            "and operands of MATMUL IntrinsicCall but found 'result: "
            "DataSymbol<UnsupportedFortranType('REAL, DIMENSION(2, 2), "
            "POINTER :: result')" in str(excinfo.value))


def test_apply_matvect(tmpdir):
    '''Test that the matmul2code apply method produces the expected
    PSyIR. We use the Fortran backend to help provide the test for
    correctness. This example includes extra indices for the vector
    and matrix arrays with these indices being references.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    root = matmul.root
    trans.apply(matmul)
    writer = FortranWriter()
    result = writer(root)
    assert (
        "subroutine my_kern()\n"
        "  integer, parameter :: idx = 3\n"
        "  real, dimension(5,10,15) :: x\n"
        "  real, dimension(10,20,10) :: y\n"
        "  real, dimension(10) :: result\n"
        "  integer :: i\n"
        "  integer :: j\n"
        "\n"
        "  do i = 1, 5, 1\n"
        "    result(i) = 0.0\n"
        "    do j = 1, 10, 1\n"
        "      result(i) = result(i) + x(i,j,idx) * y(j,idx,1)\n"
        "    enddo\n"
        "  enddo\n"
        "\n"
        "end subroutine my_kern" in result)
    assert Compile(tmpdir).string_compiles(result)


def test_apply_matvect_additional_indices(tmpdir, fortran_writer):
    '''Test that the matmul2code apply method produces the expected
    PSyIR. We use the Fortran backend to help provide the test for
    correctness. This example includes extra indices for the vector
    and matrix arrays with additional indices being literals.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    root = matmul.root
    matmul.arguments[0].children[2] = Literal("1", INTEGER_TYPE)
    matmul.arguments[1].children[1] = Literal("2", INTEGER_TYPE)
    trans.apply(matmul)
    result = fortran_writer(root)
    assert (
        "subroutine my_kern()\n"
        "  integer, parameter :: idx = 3\n"
        "  real, dimension(5,10,15) :: x\n"
        "  real, dimension(10,20,10) :: y\n"
        "  real, dimension(10) :: result\n"
        "  integer :: i\n"
        "  integer :: j\n"
        "\n"
        "  do i = 1, 5, 1\n"
        "    result(i) = 0.0\n"
        "    do j = 1, 10, 1\n"
        "      result(i) = result(i) + x(i,j,1) * y(j,2,1)\n"
        "    enddo\n"
        "  enddo\n"
        "\n"
        "end subroutine my_kern" in result)
    assert Compile(tmpdir).string_compiles(result)


def test_apply_matvect_no_indices(tmpdir, fortran_writer):
    '''Test that the matmul2code apply method produces the expected
    PSyIR. We use the Fortran backend to help provide the test for
    correctness. This example includes the array and vector being
    passed with no index information.

    '''
    trans = Matmul2CodeTrans()
    matmul = create_matmul()
    root = matmul.root
    matrix = matmul.arguments[0]
    lhs_vector = matrix.parent.parent.lhs
    matrix_symbol = matrix.symbol
    matmul.arguments[0].replace_with(Reference(matrix_symbol))
    one = Literal("1", INTEGER_TYPE)
    ten = Literal("10", INTEGER_TYPE)
    twenty = Literal("20", INTEGER_TYPE)
    matrix_symbol.datatype._shape = [
        ArrayType.ArrayBounds(one.copy(), ten.copy()),
        ArrayType.ArrayBounds(one.copy(), twenty.copy())]
    rhs_vector = matmul.arguments[1]
    rhs_vector_symbol = rhs_vector.symbol
    rhs_vector_symbol.datatype._shape = [ArrayType.ArrayBounds(one.copy(),
                                                               twenty.copy())]
    matmul.arguments[1].replace_with(Reference(rhs_vector_symbol))
    lhs_vector_symbol = lhs_vector.symbol
    lhs_vector_symbol._shape = [ArrayType.ArrayBounds(one.copy(), ten.copy())]
    lhs_vector.replace_with(Reference(lhs_vector_symbol))
    trans.apply(matmul)
    result = fortran_writer(root)
    assert (
        "subroutine my_kern()\n"
        "  integer, parameter :: idx = 3\n"
        "  real, dimension(10,20) :: x\n"
        "  real, dimension(20) :: y\n"
        "  real, dimension(10) :: result\n"
        "  integer :: i\n"
        "  integer :: j\n"
        "\n"
        "  do i = 1, 10, 1\n"
        "    result(i) = 0.0\n"
        "    do j = 1, 20, 1\n"
        "      result(i) = result(i) + x(i,j) * y(j)\n"
        "    enddo\n"
        "  enddo\n"
        "\n"
        "end subroutine my_kern" in result)
    assert Compile(tmpdir).string_compiles(result)


def test_apply_matmat_no_indices(tmpdir, fortran_reader, fortran_writer):
    '''
    Check the apply method works when the second argument to matmul is a
    matrix rather than a vector and no indices are supplied.

    '''
    psyir = fortran_reader.psyir_from_source(
        "subroutine my_sub()\n"
        "  real, dimension(2,3) :: jac\n"
        "  real, dimension(3,4) :: jac_inv\n"
        "  real, dimension(2,4) :: result\n"
        "  result = matmul(jac, jac_inv)\n"
        "end subroutine my_sub\n")
    trans = Matmul2CodeTrans()
    assign = psyir.walk(Assignment)[0]
    trans.apply(assign.rhs)
    out = fortran_writer(psyir)
    assert (
        "subroutine my_sub()\n"
        "  real, dimension(2,3) :: jac\n"
        "  real, dimension(3,4) :: jac_inv\n"
        "  real, dimension(2,4) :: result\n"
        "  integer :: i\n"
        "  integer :: j\n"
        "  integer :: ii\n"
        "\n"
        "  do j = 1, 4, 1\n"
        "    do i = 1, 2, 1\n"
        "      result(i,j) = 0.0\n"
        "      do ii = 1, 3, 1\n"
        "        result(i,j) = result(i,j) + jac(i,ii) * jac_inv(ii,j)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n"
        "\n"
        "end subroutine my_sub" in out)
    assert Compile(tmpdir).string_compiles(out)


def test_apply_matmat_extra_indices(tmpdir, fortran_reader, fortran_writer):
    '''
    Check the apply method works when the second argument to matmul is a
    matrix but additional indices are present.

    '''

    #  Extra indices in the inputs
    psyir = fortran_reader.psyir_from_source(
        "subroutine my_sub()\n"
        "  real, dimension(2,6,4) :: jac\n"
        "  real, dimension(6,3,4) :: jac_inv\n"
        "  real, dimension(2,3) :: result\n"
        "  result(:,:) = matmul(jac(:,:,1), jac_inv(:,:,2))\n"
        "end subroutine my_sub\n")
    trans = Matmul2CodeTrans()
    assign = psyir.walk(Assignment)[0]
    trans.apply(assign.rhs)
    out = fortran_writer(psyir)
    assert (
        "  real, dimension(2,6,4) :: jac\n"
        "  real, dimension(6,3,4) :: jac_inv\n"
        "  real, dimension(2,3) :: result\n"
        "  integer :: i\n"
        "  integer :: j\n"
        "  integer :: ii\n"
        "\n"
        "  do j = 1, 3, 1\n"
        "    do i = 1, 2, 1\n"
        "      result(i,j) = 0.0\n"
        "      do ii = 1, 6, 1\n"
        "        result(i,j) = result(i,j) + jac(i,ii,1) * jac_inv(ii,j,2)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n" in out)
    assert Compile(tmpdir).string_compiles(out)

    #  Extra indices in 1 input (and rhs array-notation)
    psyir = fortran_reader.psyir_from_source(
        "subroutine my_sub()\n"
        "  real, dimension(2,6) :: jac\n"
        "  real, dimension(6,3,4,5,6) :: jac_inv\n"
        "  real, dimension(2,3) :: result\n"
        "  result = matmul(jac(:,:), jac_inv(:,:,2,3,4))\n"
        "end subroutine my_sub\n")
    trans = Matmul2CodeTrans()
    assign = psyir.walk(Assignment)[0]
    trans.apply(assign.rhs)
    out = fortran_writer(psyir)
    assert (
        "  real, dimension(2,6) :: jac\n"
        "  real, dimension(6,3,4,5,6) :: jac_inv\n"
        "  real, dimension(2,3) :: result\n"
        "  integer :: i\n"
        "  integer :: j\n"
        "  integer :: ii\n"
        "\n"
        "  do j = 1, 3, 1\n"
        "    do i = 1, 2, 1\n"
        "      result(i,j) = 0.0\n"
        "      do ii = 1, 6, 1\n"
        "        result(i,j) = result(i,j) + jac(i,ii) * jac_inv(ii,j,2,3,4)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n" in out)
    assert Compile(tmpdir).string_compiles(out)

    #  Extra indices in the output
    psyir = fortran_reader.psyir_from_source(
        "subroutine my_sub()\n"
        "  real, dimension(2,6) :: jac\n"
        "  real, dimension(6,3) :: jac_inv\n"
        "  real, dimension(2,3,4,4) :: result\n"
        "  result(:,:,2,3) = matmul(jac(:,:), jac_inv(:,:))\n"
        "end subroutine my_sub\n")
    trans = Matmul2CodeTrans()
    assign = psyir.walk(Assignment)[0]
    trans.apply(assign.rhs)
    out = fortran_writer(psyir)
    assert (
        "  real, dimension(2,6) :: jac\n"
        "  real, dimension(6,3) :: jac_inv\n"
        "  real, dimension(2,3,4,4) :: result\n"
        "  integer :: i\n"
        "  integer :: j\n"
        "  integer :: ii\n"
        "\n"
        "  do j = 1, 3, 1\n"
        "    do i = 1, 2, 1\n"
        "      result(i,j,2,3) = 0.0\n"
        "      do ii = 1, 6, 1\n"
        "        result(i,j,2,3) = result(i,j,2,3) + jac(i,ii) * jac_inv(ii,j)"
        "\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n" in out)
    assert Compile(tmpdir).string_compiles(out)


def test_apply_matmat_name_clashes(tmpdir, fortran_reader, fortran_writer):
    '''
    Check the apply method works when there are already symbols present
    with names that would clash with the new loop variables.

    '''
    psyir = fortran_reader.psyir_from_source(
        "subroutine my_sub()\n"
        "  real :: i, j, ii\n"
        "  real, dimension(2,6,4) :: jac\n"
        "  real, dimension(6,3,4) :: jac_inv\n"
        "  real, dimension(2,3,2) :: result\n"
        "  result(:,:,2) = matmul(jac(:,:,1), jac_inv(:,:,2))\n"
        "end subroutine my_sub\n")
    trans = Matmul2CodeTrans()
    assign = psyir.walk(Assignment)[0]
    trans.apply(assign.rhs)
    out = fortran_writer(psyir)
    assert (
        "  real, dimension(2,6,4) :: jac\n"
        "  real, dimension(6,3,4) :: jac_inv\n"
        "  real, dimension(2,3,2) :: result\n"
        "  integer :: i_1\n"
        "  integer :: j_1\n"
        "  integer :: ii_1\n"
        "\n"
        "  do j_1 = 1, 3, 1\n"
        "    do i_1 = 1, 2, 1\n"
        "      result(i_1,j_1,2) = 0.0\n"
        "      do ii_1 = 1, 6, 1\n"
        "        result(i_1,j_1,2) = result(i_1,j_1,2) + "
        "jac(i_1,ii_1,1) * jac_inv(ii_1,j_1,2)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n" in out)
    assert Compile(tmpdir).string_compiles(out)


def test_apply_matmat_reordered(tmpdir, fortran_reader, fortran_writer):
    '''
    Check the apply method works when the second argument to matmul is a
    matrix but additional indices are present in any order.

    '''

    #  Extra indices in the inputs
    psyir = fortran_reader.psyir_from_source(
        "subroutine my_sub()\n"
        "  real, dimension(2,4,6) :: jac\n"
        "  real, dimension(6,3,4) :: jac_inv\n"
        "  real, dimension(1,2,3) :: result\n"
        "  result(1,:,:) = matmul(jac(:,2,:), jac_inv(:,:,3))\n"
        "end subroutine my_sub\n")
    trans = Matmul2CodeTrans()
    assign = psyir.walk(Assignment)[0]
    trans.apply(assign.rhs)
    out = fortran_writer(psyir)
    assert (
        "  real, dimension(2,4,6) :: jac\n"
        "  real, dimension(6,3,4) :: jac_inv\n"
        "  real, dimension(1,2,3) :: result\n"
        "  integer :: i\n"
        "  integer :: j\n"
        "  integer :: ii\n"
        "\n"
        "  do j = 1, 3, 1\n"
        "    do i = 1, 2, 1\n"
        "      result(1,i,j) = 0.0\n"
        "      do ii = 1, 6, 1\n"
        "        result(1,i,j) = result(1,i,j) + jac(i,2,ii) * jac_inv(ii,j,3)"
        "\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n" in out)
    assert Compile(tmpdir).string_compiles(out)


def test_apply_matvec_varexpr_index(tmpdir, fortran_reader, fortran_writer):
    '''
    Check the apply method works when the second argument to matmul is a
    1D slice but using a variable expression in an additional index.

    '''

    #  Extra indices in the inputs
    psyir = fortran_reader.psyir_from_source(
        "subroutine my_sub()\n"
        "  real, dimension(2,4,6) :: jac\n"
        "  real, dimension(4,6,3) :: jac_inv\n"
        "  real, dimension(2) :: result\n"
        "  integer, parameter :: arg = 0\n"
        "  result = matmul(jac(:,1,:), jac_inv(2,:,arg+3))\n"
        "end subroutine my_sub\n")
    trans = Matmul2CodeTrans()
    assign = psyir.walk(Assignment)[0]
    trans.apply(assign.rhs)
    out = fortran_writer(psyir)
    assert (
        "  integer, parameter :: arg = 0\n"
        "  real, dimension(2,4,6) :: jac\n"
        "  real, dimension(4,6,3) :: jac_inv\n"
        "  real, dimension(2) :: result\n"
        "  integer :: i\n"
        "  integer :: j\n"
        "\n"
        "  do i = 1, 2, 1\n"
        "    result(i) = 0.0\n"
        "    do j = 1, 6, 1\n"
        "      result(i) = result(i) + jac(i,1,j) * jac_inv(2,j,arg + 3)\n"
        "    enddo\n"
        "  enddo\n" in out)
    assert Compile(tmpdir).string_compiles(out)
