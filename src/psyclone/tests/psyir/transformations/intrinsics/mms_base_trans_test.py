# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Laboratory

'''Module containing tests for the mms_base_trans which is an abstract
parent class for the sum2code_trans, minval2code_trans and
maxval2code_trans transformations.

'''
import pytest

from psyclone.psyir.nodes import IntrinsicCall, Reference, Literal, Assignment
from psyclone.psyir.symbols import (
    Symbol, BOOLEAN_TYPE, INTEGER_TYPE, DataSymbol, REAL_TYPE)
from psyclone.psyir.transformations import TransformationError, Sum2CodeTrans
from psyclone.psyir.transformations.intrinsics.mms_base_trans import (
    MMSBaseTrans)
from psyclone.tests.utilities import Compile


class TestTrans(MMSBaseTrans):
    '''Utility class to allow the abstract MMSBaseTrans to be tested.'''

    def _loop_body(self, _1, _2, _3, array_ref):
        '''Minimal implementation of the abstract _loop_body method.'''
        node = Assignment.create(array_ref, Literal("33.0", REAL_TYPE))
        return node

    def _init_var(self, _):
        '''Minimal implementation of the abstract _init_var method.'''
        node = Literal("99.0", REAL_TYPE)
        return node


class NamedTestTrans(TestTrans):
    '''Utility class to allow the abstract MMSBaseTrans to be tested. Sets
    the internal intrinsic_name. Needs to use an existing intrinsic
    for the tests to work.

    '''
    _INTRINSIC_NAME = "SUM"


def test_init_exception():
    '''Check that this class can't be created as it is abstract.'''
    # pylint: disable=abstract-class-instantiated
    with pytest.raises(TypeError) as info:
        _ = MMSBaseTrans()
    assert ("Can't instantiate abstract class MMSBaseTrans with abstract "
            "methods _init_var, _loop_body" in str(info.value))


def test_get_args():
    '''Check the _get_args static method works as expected.'''
    # array
    array_reference = Reference(Symbol("array"))
    node = IntrinsicCall.create(IntrinsicCall.Intrinsic.SUM, [array_reference])
    result = MMSBaseTrans._get_args(node)
    assert result == (array_reference, None, None)

    # array, mask, dim
    mask_reference = Literal("true", BOOLEAN_TYPE)
    dim_reference = Literal("1", INTEGER_TYPE)
    node = IntrinsicCall.create(IntrinsicCall.Intrinsic.SUM, [
        array_reference.copy(), ("mask", mask_reference),
        ("dim", dim_reference)])
    result = MMSBaseTrans._get_args(node)
    assert result == (array_reference, dim_reference, mask_reference)


def test_str():
    ''' Check that the __str__ method behaves as expected. '''
    assert str(TestTrans()) == ("Convert the PSyIR None intrinsic to "
                                "equivalent PSyIR code.")
    assert str(NamedTestTrans()) == ("Convert the PSyIR SUM intrinsic to "
                                     "equivalent PSyIR code.")


# validate method

def test_validate_node():
    '''Check that an incorrect node raises the expected exception.'''
    trans = NamedTestTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert ("Error in NamedTestTrans transformation. The supplied node "
            "argument is not an intrinsic, found 'NoneType'."
            in str(info.value))

    intrinsic = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.MINVAL,
        [Reference(DataSymbol("array", REAL_TYPE))])
    with pytest.raises(TransformationError) as info:
        trans.validate(intrinsic)
    assert ("The supplied node argument is not a sum intrinsic, found "
            "'MINVAL'." in str(info.value))


def test_structure_error(fortran_reader):
    '''Test that the transformation raises an exception if the array node
    is part of a structure, as this is not currently supported.

    '''
    code = (
        "subroutine test(n,m)\n"
        "  integer :: n, m\n"
        "  type :: array_type\n"
        "      real :: array(10,10)\n"
        "  end type\n"
        "  type(array_type) :: ref\n"
        "  real :: result\n"
        "  integer :: dimension\n"
        "  result = sum(ref%array)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0].children[1]
    trans = NamedTestTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(node)
    assert ("NamedTestTrans only supports arrays or plain references for "
            "the first argument, but found 'StructureReference'."
            in str(info.value))


def test_indexed_array_error(fortran_reader):
    '''Test that the transformation raises an exception if the array node
    has a literal index, as this is invalid.

    '''
    code = (
        "subroutine test(array,n,m)\n"
        "  integer :: n, m\n"
        "  real :: array(10,10)\n"
        "  real :: result\n"
        "  integer :: dimension\n"
        "  result = sum(array(1,1))\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0].children[1]
    trans = NamedTestTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(node)
    assert ("NamedTestTrans only supports arrays with array ranges, but "
            "found a fixed dimension in 'array(1,1)'." in str(info.value))


def test_non_constant_dim_value_binop(fortran_reader):
    '''Test that the expected exception is raised if the literal value of
    the dim arg can not be determined (as it is a binary
    operation). Use the Sum2CodeTrans transformations (a subclass of
    MMSBaseTrans), as it makes it easier to raise this exception.

    '''
    code = (
        "subroutine test(array,a,b)\n"
        "  integer :: a,b\n"
        "  real :: array(10)\n"
        "  real :: result\n"
        "  result = sum(array, dim=a+b)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Sum2CodeTrans()
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.children[0].children[0].children[1]
    with pytest.raises(TransformationError) as info:
        trans.validate(node)
    assert ("Can't find the value of the 'dim' argument to the SUM "
            "intrinsic. Expected it to be a literal or a reference to "
            "a known constant value, but found 'a + b' which is type "
            "'BinaryOperation'." in str(info.value))


def test_non_constant_dim_value_ref(fortran_reader):
    '''Test that the expected exception is raised if the literal value of
    the dim arg can not be determined (as it references an unknown
    value). Use the Sum2CodeTrans transformations (a subclass of
    MMSBaseTrans), as it makes it easier to raise this exception.

    '''
    code = (
        "subroutine test(array)\n"
        "  use my_mod, only : y\n"
        "  integer :: x=y\n"
        "  real :: array(10)\n"
        "  real :: result\n"
        "  result = sum(array, dim=x)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Sum2CodeTrans()
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.children[0].children[0].children[1]
    with pytest.raises(TransformationError) as info:
        trans.validate(node)
    assert ("Can't find the value of the 'dim' argument to the SUM "
            "intrinsic. Expected it to be a literal or a reference to "
            "a known constant value, but found 'x' which is a reference "
            "to a 'DataSymbol'." in str(info.value))


def test_lhs(fortran_reader, fortran_writer, tmpdir):
    '''Test that the correct code is produced when the minval, maxval or
    sum is on the LHS of an asssignment. Uses the Sum2CodeTrans
    transformation (a subclass of MMSBaseTrans), as it is easier to
    test.

    '''
    code = (
        "subroutine test(array)\n"
        "  real :: array(10)\n"
        "  real :: result(10)\n"
        "  result(sum(array)) = 0.0\n"
        "end subroutine\n")
    expected = (
        "subroutine test(array)\n"
        "  real, dimension(10) :: array\n"
        "  real, dimension(10) :: result\n"
        "  real :: sum_var\n"
        "  integer :: i_0\n\n"
        "  sum_var = 0.0\n"
        "  do i_0 = 1, 10, 1\n"
        "    sum_var = sum_var + array(i_0)\n"
        "  enddo\n"
        "  result(sum_var) = 0.0\n\n"
        "end subroutine test\n")

    psyir = fortran_reader.psyir_from_source(code)
    trans = Sum2CodeTrans()
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.children[0].children[0].children[0].children[0]
    trans.apply(node)
    result = fortran_writer(psyir)
    assert result == expected
    assert Compile(tmpdir).string_compiles(result)


def test_array_arg(fortran_reader):
    '''Test that the expected exception is raised if the array argument is
    not an array.

    '''
    code = (
        "subroutine test(array,n,m)\n"
        "  integer :: n, m\n"
        "  real :: array\n"
        "  real :: result\n"
        "  result = sum(array)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.children[0].children[0].children[1]
    trans = NamedTestTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(node)
    assert "Expected 'array' to be an array." in str(info.value)


def test_array_shape(fortran_reader, monkeypatch):
    '''Tests that the expected exception is raised if the array range is
    not a valid value. Requires monkeypatching.

    '''
    code = (
        "subroutine test(array,n,m)\n"
        "  integer :: n, m\n"
        "  real :: array(1)\n"
        "  real :: result\n"
        "  result = sum(array)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.children[0].children[0].children[1]

    # Modify array shape from node to create exception
    array_ref = node.children[0]
    array_symbol = array_ref.symbol
    monkeypatch.setattr(array_symbol._datatype, "_shape", [None])

    trans = NamedTestTrans()
    with pytest.raises(TypeError) as info:
        trans.validate(node)
    assert ("ArrayType shape-list elements can only be 'int', "
            "ArrayType.Extent, 'DataNode' or a 2-tuple thereof but found "
            "'NoneType'." in str(info.value))


def test_unexpected_shape(fortran_reader, monkeypatch):
    '''Tests that the expected exception is raised if the array shape is
    not a valid value. Requires monkeypatching.

    '''
    code = (
        "subroutine test(array,n,m)\n"
        "  integer :: n, m\n"
        "  real :: array(1)\n"
        "  real :: result\n"
        "  result = sum(array)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.children[0].children[0].children[1]
    array_ref = node.children[0]
    # Modify the shape of the array reference shape to create an
    # exception
    monkeypatch.setattr(array_ref.symbol._datatype, "_shape", [1])

    trans = NamedTestTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(node)
    assert ("Unexpected shape for array. Expecting one of Deferred, Attribute "
            "or Bounds but found '1'." in str(info.value))


def test_array_type_arg(fortran_reader):
    '''Test that the expected exception is raised if the array is an
    unsupported datatype.

    '''
    code = (
        "subroutine test(array,n,m)\n"
        "  integer :: n, m\n"
        "  logical :: array(10)\n"
        "  real :: result\n"
        "  result = sum(array)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.children[0].children[0].children[1]
    trans = NamedTestTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(node)
    assert ("Only real and integer types supported for array 'array', "
            "but found 'BOOLEAN'." in str(info.value))


def test_not_assignment(fortran_reader):
    '''Test that the expected exception is raised if the intrinsic call is
    not part of an assignment (e.g. is an argument to a subroutine),
    as this is not currently supported.

    '''
    code = (
        "subroutine test(array)\n"
        "  integer :: array(10)\n"
        "  call routine(sum(array))\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Call/IntrinsicCall
    node = psyir.children[0].children[0].children[0]
    trans = NamedTestTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(node)
    assert ("NamedTestTrans only works when the intrinsic is part "
            "of an Assignment" in str(info.value))


# apply

@pytest.mark.parametrize("idim1,idim2,rdim11,rdim12,rdim21,rdim22",
                         [("10", "20", "1", "10", "1", "20"),
                          ("n", "m", "1", "n", "1", "m"),
                          ("0:n", "2:m", "0", "n", "2", "m"),
                          (":", ":", "LBOUND(array, dim=1)",
                           "UBOUND(array, dim=1)",
                           "LBOUND(array, dim=2)",
                           "UBOUND(array, dim=2)")])
def test_apply(idim1, idim2, rdim11, rdim12, rdim21, rdim22,
               fortran_reader, fortran_writer, tmpdir):
    '''Test that a sum intrinsic as the only term on the rhs of an
    assignment with a single array argument gets transformed as
    expected. Test with known and unknown array sizes. What we care
    about here are the sum_var array and the generated loop bounds.

    '''
    code = (
        f"subroutine test(array,n,m)\n"
        f"  integer :: n, m\n"
        f"  real :: array({idim1},{idim2})\n"
        f"  real :: result\n"
        f"  result = sum(array)\n"
        f"end subroutine\n")
    expected_decl = "  real :: sum_var\n"
    expected_bounds = (
        f"  do i_1 = {rdim21}, {rdim22}, 1\n"
        f"    do i_0 = {rdim11}, {rdim12}, 1\n")
    expected_result = "  result = sum_var\n"
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.walk(IntrinsicCall)[0]
    trans = NamedTestTrans()
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected_decl in result
    assert expected_bounds in result
    assert expected_result in result
    assert Compile(tmpdir).string_compiles(result)


def test_apply_multi(fortran_reader, fortran_writer, tmpdir):
    '''Test that a sum intrinsic as part of multiple term on the rhs of an
    assignment with a single array argument gets transformed as
    expected. What we care about here are the sum_var array and the
    generated loop bounds.

    '''
    code = (
        "subroutine test(array,n,m,value1,value2)\n"
        "  integer :: n, m\n"
        "  real :: array(n,m)\n"
        "  real :: value1, value2\n"
        "  real :: result\n"
        "  result = value1 + sum(array) * value2\n"
        "end subroutine\n")
    expected_decl = "  real :: sum_var\n"
    expected_bounds = (
        "  do i_1 = 1, m, 1\n"
        "    do i_0 = 1, n, 1\n")
    expected_result = "  result = value1 + sum_var * value2\n"
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/BinaryOperation(ADD)/
    # BinaryOperation(MUL)/IntrinsicCall
    node = psyir.walk(IntrinsicCall)[0]
    trans = NamedTestTrans()
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected_decl in result
    assert expected_bounds in result
    assert expected_result in result
    assert Compile(tmpdir).string_compiles(result)


def test_apply_dimension_1d(fortran_reader, fortran_writer, tmpdir):
    '''Test that the apply method works as expected when a dimension
    argument is specified and the array is one dimensional. This
    should be the same as if dimension were not specified at all.
    What we care about here are the sum_var array and the generated
    loop bounds.

    '''
    code = (
        "subroutine test(array,value1,value2)\n"
        "  real :: array(:)\n"
        "  real :: value1, value2\n"
        "  real :: result\n"
        "  result = value1 + sum(array,dim=1) * value2\n"
        "end subroutine\n")
    expected_decl = "  real :: sum_var\n"
    expected_bounds = "  do i_0 = LBOUND(array, dim=1), "
    expected_bounds += "UBOUND(array, dim=1), 1\n"
    expected_result = "  result = value1 + sum_var * value2\n"
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/BinaryOperation(ADD)/
    # BinaryOperation(MUL)/IntrinsicCall
    node = psyir.walk(IntrinsicCall)[0]
    trans = NamedTestTrans()
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected_decl in result
    assert expected_bounds in result
    assert expected_result in result
    assert Compile(tmpdir).string_compiles(result)


def test_apply_dimension_multid(fortran_reader, fortran_writer, tmpdir):
    '''Test that the apply method works as expected when a dimension
    argument is specified and the array is multi-dimensional. What we
    care about here are the sum_var array and the generated loop
    bounds.

    '''
    code = (
        "subroutine test(array,value1,value2,n,m,p)\n"
        "  integer :: n,m,p\n"
        "  real :: array(n,m,p)\n"
        "  real :: value1, value2\n"
        "  real :: result(n,p)\n"
        "  result(:,:) = value1 + sum(array,dim=2) * value2\n"
        "end subroutine\n")
    expected_decl = "  real, dimension(n,p) :: sum_var\n"
    expected_bounds = (
        "  do i_2 = 1, p, 1\n"
        "    do i_1 = 1, m, 1\n"
        "      do i_0 = 1, n, 1\n")
    expected_result = "  result(:,:) = value1 + sum_var(:,:) * value2\n\n"
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/BinaryOperation(ADD)/
    # BinaryOperation(MUL)/IntrinsicCall
    node = [intr for intr in psyir.walk(IntrinsicCall)
            if intr.intrinsic == IntrinsicCall.Intrinsic.SUM][0]
    trans = NamedTestTrans()
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected_decl in result
    assert expected_bounds in result
    assert expected_result in result
    assert Compile(tmpdir).string_compiles(result)


def test_apply_dimension_multid_unknown(
        fortran_reader, fortran_writer, tmpdir):
    '''Test that lbound and ubound are used to declare the sum_var
    variable and for the loop bounds if the bounds of the array are
    not known.

    '''
    code = (
        "subroutine test(array,value1,value2,result)\n"
        "  real :: array(:,:,:)\n"
        "  real :: value1, value2\n"
        "  real :: result(:,:)\n"
        "  result(:,:) = value1 + sum(array,dim=2) * value2\n"
        "end subroutine\n")
    expected_decl = (
        "  real, dimension(LBOUND(array, dim=1):UBOUND(array, dim=1),"
        "LBOUND(array, dim=3):UBOUND(array, dim=3)) :: sum_var\n")
    expected_bounds = (
        "  do i_2 = LBOUND(array, dim=3), UBOUND(array, dim=3), 1\n"
        "    do i_1 = LBOUND(array, dim=2), UBOUND(array, dim=2), 1\n"
        "      do i_0 = LBOUND(array, dim=1), UBOUND(array, dim=1), 1\n")
    expected_result = "  result(:,:) = value1 + sum_var(:,:) * value2\n\n"
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/BinaryOperation(ADD)/
    # BinaryOperation(MUL)/IntrinsicCall
    node = [intr for intr in psyir.walk(IntrinsicCall)
            if intr.intrinsic == IntrinsicCall.Intrinsic.SUM][0]
    trans = NamedTestTrans()
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected_decl in result
    assert expected_bounds in result
    assert expected_result in result
    assert Compile(tmpdir).string_compiles(result)


def test_apply_dimension_multid_range(fortran_reader, fortran_writer, tmpdir):
    '''Test that the apply method works as expected when an array range is
    specified and the array is multi-dimensional. What we
    care about here are the sum_var array and the generated loop
    bounds.

    '''
    code = (
        "subroutine test(array,value1,value2,n,m,p)\n"
        "  integer :: n,m,p\n"
        "  real :: array(:,:,:)\n"
        "  real :: value1, value2\n"
        "  real :: result(n,p)\n"
        "  result(:,:) = value1 + sum(array(1:n,m-1:m,1:p),dim=2) * "
        "value2\n"
        "end subroutine\n")
    expected_decl = "  real, dimension(n,p) :: sum_var\n"
    expected_bounds = (
        "  do i_2 = 1, p, 1\n"
        "    do i_1 = m - 1, m, 1\n"
        "      do i_0 = 1, n, 1\n")
    expected_result = "  result(:,:) = value1 + sum_var(:,:) * value2\n\n"
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/BinaryOperation(ADD)/
    # BinaryOperation(MUL)/IntrinsicCall
    node = [intr for intr in psyir.walk(IntrinsicCall)
            if intr.intrinsic == IntrinsicCall.Intrinsic.SUM][0]
    trans = NamedTestTrans()
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected_decl in result
    assert expected_bounds in result
    assert expected_result in result
    assert Compile(tmpdir).string_compiles(result)


def test_mask(fortran_reader, fortran_writer, tmpdir):
    '''Test that the transformation works when there is a mask specified.
    What we care about here are the sum_var array, the generated loop
    bounds and the mask.

    '''
    code = (
        "program test\n"
        "  real :: array(10,10)\n"
        "  real :: result\n"
        "  result = sum(array, mask=MOD(array, 2.0)==1)\n"
        "end program\n")
    # We are using the NamedTestTrans subclass here which simply sets
    # the value of the array to 33.
    expected = (
        "  do i_1 = 1, 10, 1\n"
        "    do i_0 = 1, 10, 1\n"
        "      if (MOD(array(i_0,i_1), 2.0) == 1) then\n"
        "        array(i_0,i_1) = 33.0\n"
        "      end if\n"
        "    enddo\n"
        "  enddo\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.walk(IntrinsicCall)[0]
    trans = NamedTestTrans()
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_mask_dimension(fortran_reader, fortran_writer, tmpdir):
    '''Test that the transformation works when there is a mask and a
    dimension specified. What we care about here are the sum_var
    array, the generated loop bounds and the mask.

    '''
    code = (
        "program test\n"
        "  real :: array(10,10)\n"
        "  real :: result(10)\n"
        "  integer, parameter :: dimension=2\n"
        "  result = sum(array, dimension, mask=MOD(array, 2.0)==1)\n"
        "end program\n")
    expected_decl = "  real, dimension(10) :: sum_var\n"
    # We are using the NamedTestTrans subclass here which simply sets
    # the value of the array to 33.
    expected_bounds = (
        "  do i_1 = 1, 10, 1\n"
        "    do i_0 = 1, 10, 1\n"
        "      if (MOD(array(i_0,i_1), 2.0) == 1) then\n"
        "        array(i_0,i_1) = 33.0\n"
        "      end if\n")
    expected_result = "  result = sum_var(:)\n"
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.walk(IntrinsicCall)[0]
    trans = NamedTestTrans()
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected_decl in result
    assert expected_bounds in result
    assert expected_result in result
    assert Compile(tmpdir).string_compiles(result)


def test_mask_array_indexed(fortran_reader, fortran_writer, tmpdir):
    '''Test that the mask code works if the array iself it used as part of
    the mask. In this case it will already be indexed. Use the
    Sum2CodeTrans transformation (a subclass of MMSBaseTrans), as it
    is easier to test.

    '''
    code = (
        "program sum_test\n"
        "  integer :: a(4)\n"
        "  integer :: result\n"
        "  a(1) = 2\n"
        "  a(2) = 1\n"
        "  a(3) = 2\n"
        "  a(4) = 1\n"
        "  result = sum(a, mask=a(1)>a)\n"
        "end program\n")
    expected = (
        "program sum_test\n"
        "  integer, dimension(4) :: a\n"
        "  integer :: result\n"
        "  integer :: sum_var\n"
        "  integer :: i_0\n\n"
        "  a(1) = 2\n"
        "  a(2) = 1\n"
        "  a(3) = 2\n"
        "  a(4) = 1\n"
        "  sum_var = 0\n"
        "  do i_0 = 1, 4, 1\n"
        "    if (a(1) > a(i_0)) then\n"
        "      sum_var = sum_var + a(i_0)\n"
        "    end if\n"
        "  enddo\n"
        "  result = sum_var\n\n"
        "end program sum_test\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Sum2CodeTrans()
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.walk(IntrinsicCall)[0]
    trans.apply(node)
    result = fortran_writer(psyir)
    assert result == expected
    assert Compile(tmpdir).string_compiles(result)


def test_allocate_dim(fortran_reader, fortran_writer, tmpdir):
    '''Test that a newly created array is allocated after the original
    array is allocated (if the original array is allocated). Use the
    Sum2CodeTrans transformations (a subclass of MMSBaseTrans), as it
    is easier to test.

    '''
    code = (
        "program sum_test\n"
        "  integer, allocatable :: a(:,:,:)\n"
        "  integer :: result(4,4)\n"
        "  allocate(a(4,4,4))\n"
        "  result = sum(a, dim=2)\n"
        "  deallocate(a)\n"
        "end program\n")
    expected = (
        "program sum_test\n"
        "  integer, allocatable, dimension(:,:,:) :: a\n"
        "  integer, dimension(4,4) :: result\n"
        "  integer, allocatable, dimension(:,:) :: sum_var\n"
        "  integer :: i_0\n"
        "  integer :: i_1\n"
        "  integer :: i_2\n\n"
        "  ALLOCATE(a(1:4,1:4,1:4))\n"
        "  ALLOCATE(sum_var(LBOUND(a, dim=1):UBOUND(a, dim=1),"
        "LBOUND(a, dim=3):UBOUND(a, dim=3)))\n"
        "  sum_var(:,:) = 0\n"
        "  do i_2 = LBOUND(a, dim=3), UBOUND(a, dim=3), 1\n"
        "    do i_1 = LBOUND(a, dim=2), UBOUND(a, dim=2), 1\n"
        "      do i_0 = LBOUND(a, dim=1), UBOUND(a, dim=1), 1\n"
        "        sum_var(i_0,i_2) = sum_var(i_0,i_2) + a(i_0,i_1,i_2)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n"
        "  result = sum_var(:,:)\n"
        "  DEALLOCATE(a)\n\n"
        "end program sum_test\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Sum2CodeTrans()
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.walk(IntrinsicCall)[1]
    trans.apply(node)
    result = fortran_writer(psyir)
    assert result == expected
    assert Compile(tmpdir).string_compiles(result)
