# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Lab

'''Module containing tests for the DotProduct2CodeTrans
transformation.

'''
import pytest

from psyclone.psyir.nodes import BinaryOperation
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.transformations.intrinsics.dotproduct2code_trans import \
    DotProduct2CodeTrans
from psyclone.tests.utilities import Compile


def check(code, expected, fortran_reader, fortran_writer, tmpdir, index=0):
    ''' xxx '''
    psyir = fortran_reader.psyir_from_source(code)
    dot_product = psyir.walk(BinaryOperation)[index]
    assert dot_product.operator == BinaryOperation.Operator.DOT_PRODUCT
    trans = DotProduct2CodeTrans()
    trans.apply(dot_product)
    result = fortran_writer(psyir)
    print(code)
    print(result)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


# bounds [TBD]

# DotProduct2CodeTrans:init

# TODO calls super?

def test_initialise():
    '''Check that the class DotProduct2CodeTrans behaves as expected when
    an instance of the class is created.

    '''
    trans = DotProduct2CodeTrans()
    assert trans._operator_name == "DOTPRODUCT"
    assert (str(trans) == "Convert the PSyIR DOTPRODUCT intrinsic to "
            "equivalent PSyIR code.")
    assert trans.name == "Dotproduct2CodeTrans"


# DotProduct2CodeTrans:validate

# TODO calls super (e.g. correct node type)

def test_validate_references(fortran_reader):
    '''Test that the dotproduct2code validate method produces the expected
    exception when at least one of the arguments is not an array. In
    this case it is a matmul intrinsic.

    '''
    code = (
        f"subroutine dot_product_test(v1,v2,a3)\n"
        f"real,intent(in) :: v1(:), v2(:), a3(:,:)\n"
        f"real :: result\n"
        f"result = dot_product(matmul(a3,v1),v2)\n"
        f"end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    dot_product = psyir.walk(BinaryOperation)[0]
    assert dot_product.operator == BinaryOperation.Operator.DOT_PRODUCT
    trans = DotProduct2CodeTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(dot_product)
    assert ("The dotproduct2code_trans transformation only supports the "
            "transformation of a dotproduct intrinsic if its arguments "
            "are arrays, but found MATMUL(a3, v1) in "
            "DOT_PRODUCT(MATMUL(a3, v1), v2)" in str(info.value))


def test_validate_1d_array(fortran_reader):
    '''Test that the dotproduct2code validate method produces the expected
    exception when at least one of the arguments does not use array
    slice notation but is not known to be a 1D array.

    '''
    code = (
        f"subroutine dot_product_test(a1,a2)\n"
        f"real,intent(in) :: a1(:,:), a2(:,:)\n"
        f"real :: result\n"
        f"result = dot_product(a1,a2)\n"
        f"end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    dot_product = psyir.walk(BinaryOperation)[0]
    assert dot_product.operator == BinaryOperation.Operator.DOT_PRODUCT
    trans = DotProduct2CodeTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(dot_product)
    assert ("The dotproduct2code_trans transformation only supports the "
            "transformation of a dotproduct intrinsic with an argument not "
            "containing an array slice if the argument is a 1D array, but "
            "found a1 in DOT_PRODUCT(a1, a2)." in str(info.value))


def test_validate_array_slice_dim1(fortran_reader):
    '''Test that the dotproduct2code validate method produces the expected
    exception when at least one of the arguments uses array slice
    notation but the array slice is not used in the the first
    dimension of the array.

    '''
    code = (
        f"subroutine dot_product_test(a1,a2)\n"
        f"real,intent(in) :: a1(:,:), a2(:,:)\n"
        f"real :: result\n"
        f"result = dot_product(a1(:,1),a2(1,:))\n"
        f"end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    dot_product = psyir.walk(BinaryOperation)[0]
    assert dot_product.operator == BinaryOperation.Operator.DOT_PRODUCT
    trans = DotProduct2CodeTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(dot_product)
    assert ("The dotproduct2code_trans transformation only supports the "
            "transformation of a dotproduct intrinsic with an argument "
            "containing an array slice if the array slice is for the 1st "
            "dimension of the array, but found a2(1,:) in "
            "DOT_PRODUCT(a1(:,1), a2(1,:))." in str(info.value))


def test_validate_array_full_slice(fortran_reader):
    '''Test that the dotproduct2code validate method produces the expected
    exception when at least one of the arguments uses array slice
    notation but the array slice is not for the full range of the
    dimension.

    '''
    code = (
        f"subroutine dot_product_test(a1,a2)\n"
        f"real,intent(in) :: a1(:,:), a2(:,:)\n"
        f"real :: result\n"
        f"result = dot_product(a1(2:4,1),a2(:,10))\n"
        f"end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    dot_product = psyir.walk(BinaryOperation)[0]
    assert dot_product.operator == BinaryOperation.Operator.DOT_PRODUCT
    trans = DotProduct2CodeTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(dot_product)
    assert ("The dotproduct2code_trans transformation only supports the "
            "transformation of a dotproduct intrinsic with an argument not "
            "an array slice if the argument is for the 1st dimension of "
            "the array and is for the full range of that dimension, but "
            "found a1(2:4,1) in DOT_PRODUCT(a1(2:4,1), a2(:,10))."
            in str(info.value))


# DotProduct2CodeTrans:apply

# TODO calls validate test

@pytest.mark.parametrize("dim1,dim2", [("10", "10"), (":", "10"), ("10", ":")])
def test_apply_known_dims(tmpdir, fortran_reader, fortran_writer, dim1, dim2):
    '''Test that the dotproduct2code apply method produces the expected
    PSyIR when at least one of the vectors has a known dimension.

    '''
    code = (
        f"subroutine dot_product_test(v1,v2)\n"
        f"real,intent(in) :: v1({dim1}), v2({dim2})\n"
        f"real :: result\n"
        f"result = dot_product(v1,v2)\n"
        f"end subroutine\n")
    expected = (
        "  integer :: i\n"
        "  real :: res_dot_product\n\n"
        "  res_dot_product = 0.0\n"
        "  do i = 1, 10, 1\n"
        "    res_dot_product = res_dot_product + v1(i) * v2(i)\n"
        "  enddo\n"
        "  result = res_dot_product\n\n")
    check(code, expected, fortran_reader, fortran_writer, tmpdir)


def test_apply_unknown_dims(tmpdir, fortran_reader, fortran_writer):
    '''Test that the dotproduct2code apply method produces the expected
    PSyIR when neither of the vectors have a known size.

    '''
    code = (
        "subroutine dot_product_test(v1,v2)\n"
        "real,intent(in) :: v1(:), v2(:)\n"
        "real :: result\n"
        "result = dot_product(v1,v2)\n"
        "end subroutine\n")
    expected = (
        "  integer :: i\n"
        "  real :: res_dot_product\n\n"
        "  res_dot_product = 0.0\n"
        "  do i = LBOUND(v1, 1), UBOUND(v1, 1), 1\n"
        "    res_dot_product = res_dot_product + v1(i) * v2(i)\n"
        "  enddo\n"
        "  result = res_dot_product\n\n")
    check(code, expected, fortran_reader, fortran_writer, tmpdir)


def test_apply_multi_rhs(tmpdir, fortran_reader, fortran_writer):
    '''Test that the dotproduct2code apply method produces the expected
    PSyIR when the expression on the rhs contains more than just the
    DOT_PRODUCT.

    '''
    code = (
        "subroutine dot_product_test(v1,v2)\n"
        "real,intent(in) :: v1(10), v2(:)\n"
        "real :: a, b, c, result\n"
        "result = a + b*dot_product(v1,v2) + c\n"
        "end subroutine\n")
    expected = (
        "  integer :: i\n"
        "  real :: res_dot_product\n\n"
        "  res_dot_product = 0.0\n"
        "  do i = 1, 10, 1\n"
        "    res_dot_product = res_dot_product + v1(i) * v2(i)\n"
        "  enddo\n"
        "  result = a + b * res_dot_product + c\n\n")
    check(code, expected, fortran_reader, fortran_writer, tmpdir, index=3)


@pytest.mark.parametrize("arg1,arg2",[("", "(:)"), ("(:)", ""),
                                      ("(:)", "(:)")])
def test_apply_array_notation(
        tmpdir, fortran_reader, fortran_writer, arg1, arg2):
    '''Test that the dotproduct2code apply method produces the expected
    PSyIR when array notation is used on the rhs by one or both of the
    dot_product arguments.

    '''
    code = (
        f"subroutine dot_product_test(v1,v2)\n"
        f"real,intent(in) :: v1(:), v2(:)\n"
        f"real :: result\n"
        f"result = dot_product(v1{arg1},v2{arg2})\n"
        f"end subroutine\n")
    expected = (
        "  integer :: i\n"
        "  real :: res_dot_product\n\n"
        "  res_dot_product = 0.0\n"
        "  do i = LBOUND(v1, 1), UBOUND(v1, 1), 1\n"
        "    res_dot_product = res_dot_product + v1(i) * v2(i)\n"
        "  enddo\n"
        "  result = res_dot_product\n\n")
    check(code, expected, fortran_reader, fortran_writer, tmpdir)


@pytest.mark.parametrize("arg1,arg2,res1,res2",
                         [("(:,n)", "(:,1)", "(i,n)", "(i,1)")])
def test_apply_extra_dims(tmpdir, fortran_reader, fortran_writer, arg1, arg2,
                          res1, res2):
    '''Test that the dotproduct2code apply method produces the expected
    PSyIR when the supplied dot_product arguments are arrays with
    different dimensions having array notation.

    '''
    code = (
        f"subroutine dot_product_test(v1,v2)\n"
        f"integer :: n\n"
        f"real,intent(in) :: v1(:,:), v2(:,:)\n"
        f"real :: result\n"
        f"result = dot_product(v1{arg1},v2{arg2})\n"
        f"end subroutine\n")
    expected = (
        f"  integer :: i\n"
        f"  real :: res_dot_product\n\n"
        f"  res_dot_product = 0.0\n"
        f"  do i = LBOUND(v1, 1), UBOUND(v1, 1), 1\n"
        f"    res_dot_product = res_dot_product + v1{res1} * v2{res2}\n"
        f"  enddo\n"
        f"  result = res_dot_product\n\n")
    check(code, expected, fortran_reader, fortran_writer, tmpdir)
