# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council
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
# Modified: S. Siso, STFC Daresbury Lab

'''Module containing tests for the DotProduct2CodeTrans
transformation.

Note, there are no tests for invalid argument combinations to
dot_product (e.g. different precision or different datatypes) as this
is not checked in the validate() method in DotProduct2CodeTrans. This
is because invalid argument combinations should have already been
picked up when creating the PSyIR.

'''
import pytest

from psyclone.psyir.nodes import IntrinsicCall
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.transformations.intrinsics.dotproduct2code_trans import \
    DotProduct2CodeTrans, _get_array_bound
from psyclone.tests.utilities import Compile


# Utilities

def check_validate(code, expected, fortran_reader):
    '''Utility function that takes Fortran code that is unsupported by the
    DotProduct2CodeTrans transformation and checks that the expected
    exception is raised when the validate function is applied.

    :param str code: the tangent linear code.
    :param str expected: the expected exception text.
    :param fortran_reader: converts Fortran into PSyIR.
    :type fortran_reader: :py:class:`psyclone.psyir.frontend.fortran`

    '''
    psyir = fortran_reader.psyir_from_source(code)
    trans = DotProduct2CodeTrans()
    for intrinsic in psyir.walk(IntrinsicCall):
        if intrinsic.intrinsic == IntrinsicCall.Intrinsic.DOT_PRODUCT:
            with pytest.raises(TransformationError) as info:
                trans.validate(intrinsic)
            assert expected in str(info.value)


def check_trans(code, expected, fortran_reader, fortran_writer, tmpdir):
    '''Utility function that takes fortran code and checks that the
    expected code is produced when the DotProduct2CodeTrans
    transformation is applied. The code is also checked to see if it
    will compile.

    :param str code: the tangent linear code.
    :param str expected: the expected adjoint code.
    :param fortran_reader: converts Fortran into PSyIR.
    :type fortran_reader: :py:class:`psyclone.psyir.frontend.fortran`
    :param fortran_writer: converts PSyIR into Fortran.
    :type fortran_writer: :py:class:`psyclone.psyir.backend.fortran`
    :param tmpdir: path to a test-specific temporary directory in \
        which to test compilation.
    :type tmpdir: :py:class:`py._path.local.LocalPath`

    '''
    psyir = fortran_reader.psyir_from_source(code)
    trans = DotProduct2CodeTrans()
    for intrinsic in psyir.walk(IntrinsicCall):
        if intrinsic.intrinsic == IntrinsicCall.Intrinsic.DOT_PRODUCT:
            trans.apply(intrinsic)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


# _get_array_bound function

@pytest.mark.parametrize("dim1,dim2", [("2:10", "2:10"), (":", "2:10"),
                                       ("2:10", ":")])
def test_bound_explicit(fortran_reader, dim1, dim2):
    '''Test that explicit bounds are returned if at least one argument is
    declared with explicit bounds.

    '''
    code = (
        f"subroutine dot_product_test(v1,v2)\n"
        f"real,intent(in) :: v1({dim1}), v2({dim2})\n"
        f"real :: result\n"
        f"result = dot_product(v1,v2)\n"
        f"end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    dot_product = psyir.walk(IntrinsicCall)[0]
    assert dot_product.intrinsic == IntrinsicCall.Intrinsic.DOT_PRODUCT
    lower, upper, step = _get_array_bound(
        dot_product.children[0], dot_product.children[1])
    assert lower.value == '2'
    assert upper.value == '10'
    assert step.value == '1'


def test_bound_unknown(fortran_reader, fortran_writer):
    '''Test that range bounds are returned if neither argument is declared
    with explicit bounds.

    '''
    code = (
        "subroutine dot_product_test(v1,v2)\n"
        "real,intent(in) :: v1(:), v2(:)\n"
        "real :: result\n"
        "result = dot_product(v1,v2)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    dot_product = psyir.walk(IntrinsicCall)[0]
    assert dot_product.intrinsic == IntrinsicCall.Intrinsic.DOT_PRODUCT
    lower, upper, step = _get_array_bound(
        dot_product.children[0], dot_product.children[1])
    assert 'LBOUND(v1, dim=1)' in fortran_writer(lower)
    assert 'UBOUND(v1, dim=1)' in fortran_writer(upper)
    assert step.value == '1'


# DotProduct2CodeTrans class init method

def test_initialise():
    '''Check that the class DotProduct2CodeTrans behaves as expected when
    an instance of the class is created. Note, this also tests that
    the parent constructor is also called as this sets the name and
    value of __str__.

    '''
    trans = DotProduct2CodeTrans()
    assert (str(trans) == "Convert the PSyIR 'DOT_PRODUCT' intrinsic to "
            "equivalent PSyIR code.")
    assert trans.name == "DotProduct2CodeTrans"


# DotProduct2CodeTrans class validate method

def test_validate_super():
    '''Test that the DotProduct2CodeTrans validate method calls the validate
    method of the parent class.

    '''
    trans = DotProduct2CodeTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert ("The supplied node must be an 'IntrinsicCall', but found "
            "'NoneType'." in str(info.value))


def test_validate_references_matmul(fortran_reader):
    '''Test that the DotProduct2CodeTrans validate method produces the
    expected exception when at least one of the arguments is not an
    array. In this case it is a matmul intrinsic.

    '''
    code = (
        "subroutine dot_product_test(v1,v2,a3)\n"
        "real,intent(in) :: v1(:), v2(:), a3(:,:)\n"
        "real :: result\n"
        "result = dot_product(matmul(a3,v1),v2)\n"
        "end subroutine\n")
    expected = (
        "The DotProduct2CodeTrans transformation only supports the "
        "transformation of a dotproduct intrinsic if its arguments "
        "are plain arrays, but found MATMUL(a3, v1) in "
        "DOT_PRODUCT(MATMUL(a3, v1), v2)")
    check_validate(code, expected, fortran_reader)


def test_validate_references_structure(fortran_reader):
    '''Test that the DotProduct2CodeTrans validate method produces the
    expected exception when at least one of the arguments is an array
    reference within a structure. This is potentially valid but is not
    currently supported.

    '''
    code = (
        "subroutine dot_product_test()\n"
        "type :: grid_type\n"
        "  real :: var1(10)\n"
        "  real :: var2(10)\n"
        "end type\n"
        "type(grid_type) :: grid\n"
        "real :: result\n"
        "result = dot_product(grid%var1(:),grid%var2(:))\n"
        "end subroutine\n")
    expected = (
        "The DotProduct2CodeTrans transformation only supports the "
        "transformation of a dotproduct intrinsic if its arguments are plain "
        "arrays, but found grid%var1(:) in DOT_PRODUCT(grid%var1(:), "
        "grid%var2(:)).")
    check_validate(code, expected, fortran_reader)


def test_validate_1d_array(fortran_reader):
    '''Test that the DotProduct2CodeTrans validate method produces the
    expected exception when at least one of the arguments does not use
    array slice notation but is not known to be a 1D array.

    '''
    code = (
        "subroutine dot_product_test(a1,a2)\n"
        "real,intent(in) :: a1(:,:), a2(:,:)\n"
        "real :: result\n"
        "result = dot_product(a1,a2)\n"
        "end subroutine\n")
    expected = (
        "The DotProduct2CodeTrans transformation only supports the "
        "transformation of a dotproduct intrinsic with an argument not "
        "containing an array slice if the argument is a 1D array, but "
        "found a1 with 2 dimensions in DOT_PRODUCT(a1, a2).")
    check_validate(code, expected, fortran_reader)


def test_validate_array_slice_dim1(fortran_reader):
    '''Test that the DotProduct2CodeTrans validate method produces the
    expected exception when at least one of the arguments uses array
    slice notation but the array slice is not used in the the first
    dimension of the array.

    '''
    code = (
        "subroutine dot_product_test(a1,a2)\n"
        "real,intent(in) :: a1(:,:), a2(:,:)\n"
        "real :: result\n"
        "result = dot_product(a1(:,1),a2(1,:))\n"
        "end subroutine\n")
    expected = (
        "The DotProduct2CodeTrans transformation only supports the "
        "transformation of a dotproduct intrinsic with an argument "
        "containing an array slice if the array slice is for the 1st "
        "dimension of the array, but found a2(1,:) in "
        "DOT_PRODUCT(a1(:,1), a2(1,:)).")
    check_validate(code, expected, fortran_reader)


def test_validate_array_full_slice(fortran_reader):
    '''Test that the DotProduct2CodeTrans validate method produces the
    expected exception when at least one of the arguments uses array
    slice notation but the array slice is not for the full range of
    the dimension.

    '''
    code = (
        "subroutine dot_product_test(a1,a2)\n"
        "real,intent(in) :: a1(:,:), a2(:,:)\n"
        "real :: result\n"
        "result = dot_product(a1(2:4,1),a2(:,10))\n"
        "end subroutine\n")
    expected = (
        "The DotProduct2CodeTrans transformation only supports the "
        "transformation of a dotproduct intrinsic with an argument containing "
        "an array slice if the argument is for the 1st dimension of the array "
        "and is for the full range of that dimension, but found a1(2:4,1) in "
        "DOT_PRODUCT(a1(2:4,1), a2(:,10)).")
    check_validate(code, expected, fortran_reader)


def test_validate_real(fortran_reader):
    '''Test that the DotProduct2CodeTrans validate method raises the
    expected exception if the datatype of the arguments is not
    real.

    '''
    code = (
        "subroutine dot_product_test(v1,v2)\n"
        "integer,intent(in) :: v1(:), v2(:)\n"
        "integer :: result\n"
        "result = dot_product(v1,v2)\n"
        "end subroutine\n")
    expected = (
        "The DotProduct2CodeTrans transformation only supports arrays of "
        "real data, but found v1 of type INTEGER in DOT_PRODUCT(v1, v2).")
    check_validate(code, expected, fortran_reader)


# DotProduct2CodeTrans class apply method

def test_apply_calls_validate():
    '''Test that the DotProduct2CodeTrans apply method calls the validate
    method.

    '''
    trans = DotProduct2CodeTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert ("The supplied node must be an 'IntrinsicCall', but found "
            "'NoneType'." in str(info.value))


@pytest.mark.parametrize("dim1,dim2,upper_bound",
                         [("10", "10", "10"), (":", "10", "10"),
                          ("10", ":", "10"), ("10", "n", "10"),
                          ("n", "10", "n"), (":", "n", "n"),
                          ("n", ":", "n")])
def test_apply_known_dims(
        tmpdir, fortran_reader, fortran_writer, dim1, dim2, upper_bound):
    '''Test that the DotProduct2CodeTrans apply method produces the
    expected PSyIR when at least one of the vectors has a known
    dimension. Also check that the correct precision is used.

    '''
    code = (
        f"subroutine dot_product_test(v1,v2,n)\n"
        f"integer,intent(in) :: n\n"
        f"real*4,intent(in) :: v1({dim1}), v2({dim2})\n"
        f"real*4 :: result\n"
        f"result = dot_product(v1,v2)\n"
        f"end subroutine\n")
    expected = (
        f"  integer :: i\n"
        f"  real*4 :: res_dot_product\n\n"
        f"  res_dot_product = 0.0\n"
        f"  do i = 1, {upper_bound}, 1\n"
        f"    res_dot_product = res_dot_product + v1(i) * v2(i)\n"
        f"  enddo\n"
        f"  result = res_dot_product\n\n")
    check_trans(code, expected, fortran_reader, fortran_writer, tmpdir)


def test_apply_unknown_dims(tmpdir, fortran_reader, fortran_writer):
    '''Test that the DotProduct2CodeTrans apply method produces the
    expected PSyIR when neither of the vectors have a known size. Also
    check that the correct precision is used.

    '''
    code = (
        "subroutine dot_product_test(v1,v2)\n"
        "integer, parameter :: r_def=4\n"
        "real(kind=r_def),intent(in) :: v1(:), v2(:)\n"
        "real(kind=r_def) :: result\n"
        "result = dot_product(v1,v2)\n"
        "end subroutine\n")
    expected = (
        "  real(kind=r_def) :: result\n"
        "  integer :: i\n"
        "  real(kind=r_def) :: res_dot_product\n\n"
        "  res_dot_product = 0.0\n"
        "  do i = LBOUND(v1, dim=1), UBOUND(v1, dim=1), 1\n"
        "    res_dot_product = res_dot_product + v1(i) * v2(i)\n"
        "  enddo\n"
        "  result = res_dot_product\n\n")
    check_trans(code, expected, fortran_reader, fortran_writer, tmpdir)


def test_apply_multi_rhs(tmpdir, fortran_reader, fortran_writer):
    '''Test that the DotProduct2CodeTrans apply method produces the
    expected PSyIR when the expression on the rhs contains more than
    just the DOT_PRODUCT.

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
    check_trans(code, expected, fortran_reader, fortran_writer, tmpdir)


@pytest.mark.parametrize("arg1,arg2", [("", "(:)"), ("(:)", ""),
                                       ("(:)", "(:)")])
def test_apply_array_notation(
        tmpdir, fortran_reader, fortran_writer, arg1, arg2):
    '''Test that the DotProduct2CodeTrans apply method produces the
    expected PSyIR when array notation is used on the rhs by one or
    both of the dot_product arguments.

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
        "  do i = LBOUND(v1, dim=1), UBOUND(v1, dim=1), 1\n"
        "    res_dot_product = res_dot_product + v1(i) * v2(i)\n"
        "  enddo\n"
        "  result = res_dot_product\n\n")
    check_trans(code, expected, fortran_reader, fortran_writer, tmpdir)


@pytest.mark.parametrize("arg1,arg2,res1,res2",
                         [("(:,n)", "(:,1)", "(i,n)", "(i,1)")])
def test_apply_extra_dims(tmpdir, fortran_reader, fortran_writer, arg1, arg2,
                          res1, res2):
    '''Test that the DotProduct2CodeTrans apply method produces the
    expected PSyIR when the supplied dot_product arguments are arrays
    with array notation and additional dimensions using literals and
    variables.

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
        f"  do i = LBOUND(v1, dim=1), UBOUND(v1, dim=1), 1\n"
        f"    res_dot_product = res_dot_product + v1{res1} * v2{res2}\n"
        f"  enddo\n"
        f"  result = res_dot_product\n\n")
    check_trans(code, expected, fortran_reader, fortran_writer, tmpdir)


@pytest.mark.parametrize(
    "dims1,dims2,arg1,arg2,res1,res2",
    [("(:,:,:)", "(:)", "(:,n,1)", "(:)", "(i,n,1)", "(i)"),
     ("(:)", "(:,:,:,:)", "(:)", "(:,1,n,2)", "(i)", "(i,1,n,2)")])
def test_apply_extra_dims_sizes(tmpdir, fortran_reader, fortran_writer,
                                dims1, dims2, arg1, arg2, res1, res2):
    '''Test that the DotProduct2CodeTrans apply method produces the
    expected PSyIR when the supplied dot_product arguments are arrays
    with array notation and additional dimensions of different sizes.

    '''
    code = (
        f"subroutine dot_product_test(v1,v2)\n"
        f"integer :: n\n"
        f"real,intent(in) :: v1{dims1}, v2{dims2}\n"
        f"real :: result\n"
        f"result = dot_product(v1{arg1},v2{arg2})\n"
        f"end subroutine\n")
    expected = (
        f"  integer :: i\n"
        f"  real :: res_dot_product\n\n"
        f"  res_dot_product = 0.0\n"
        f"  do i = LBOUND(v1, dim=1), UBOUND(v1, dim=1), 1\n"
        f"    res_dot_product = res_dot_product + v1{res1} * v2{res2}\n"
        f"  enddo\n"
        f"  result = res_dot_product\n\n")
    check_trans(code, expected, fortran_reader, fortran_writer, tmpdir)


def test_apply_multi_same_line(tmpdir, fortran_reader, fortran_writer):
    '''Test that the DotProduct2CodeTrans apply method produces the
    expected PSyIR when the code contains more than one dot_product
    and they are both on the same line.

    '''
    code = (
        "subroutine dot_product_test(v1,v2)\n"
        "real,intent(in) :: v1(10), v2(:)\n"
        "real :: a, b, c, result\n"
        "result = a + b*dot_product(v1,v2) + c*dot_product(v2(:),v1(:))\n"
        "end subroutine\n")
    expected = (
        "  integer :: i\n"
        "  real :: res_dot_product\n"
        "  integer :: i_1\n"
        "  real :: res_dot_product_1\n\n"
        "  res_dot_product = 0.0\n"
        "  do i = 1, 10, 1\n"
        "    res_dot_product = res_dot_product + v1(i) * v2(i)\n"
        "  enddo\n"
        "  res_dot_product_1 = 0.0\n"
        "  do i_1 = 1, 10, 1\n"
        "    res_dot_product_1 = res_dot_product_1 + v2(i_1) * v1(i_1)\n"
        "  enddo\n"
        "  result = a + b * res_dot_product + c * res_dot_product_1\n")
    check_trans(code, expected, fortran_reader, fortran_writer, tmpdir)


def test_apply_multi_different_line(tmpdir, fortran_reader, fortran_writer):
    '''Test that the DotProduct2CodeTrans apply method produces the
    expected PSyIR when the code contains more than one dot_product
    and they are on different lines.

    '''
    code = (
        "subroutine dot_product_test(v1,v2)\n"
        "real,intent(in) :: v1(10), v2(:)\n"
        "real :: a, b, c, result\n"
        "result = a + b*dot_product(v1,v2)\n"
        "result = result + c*dot_product(v2(:),v1(:))\n"
        "end subroutine\n")
    expected = (
        "  integer :: i\n"
        "  real :: res_dot_product\n"
        "  integer :: i_1\n"
        "  real :: res_dot_product_1\n\n"
        "  res_dot_product = 0.0\n"
        "  do i = 1, 10, 1\n"
        "    res_dot_product = res_dot_product + v1(i) * v2(i)\n"
        "  enddo\n"
        "  result = a + b * res_dot_product\n"
        "  res_dot_product_1 = 0.0\n"
        "  do i_1 = 1, 10, 1\n"
        "    res_dot_product_1 = res_dot_product_1 + v2(i_1) * v1(i_1)\n"
        "  enddo\n"
        "  result = result + c * res_dot_product_1\n")
    check_trans(code, expected, fortran_reader, fortran_writer, tmpdir)


def test_apply_explicit_range(fortran_reader, fortran_writer, tmpdir):
    '''Check that this transformation works when there is an explicit
    range declaration (i.e. wind(1:3)) that matches the array extent.

    '''
    code = (
        "subroutine dot_product_test(basis_w1)\n"
        "real, dimension(3) :: wind\n"
        "real :: basis_w1(:)\n"
        "integer :: result\n"
        "result = dot_product(basis_w1(:),wind(1:3))\n"
        "end subroutine\n")
    expected = (
        "subroutine dot_product_test(basis_w1)\n"
        "  real, dimension(:) :: basis_w1\n"
        "  real, dimension(3) :: wind\n"
        "  integer :: result\n"
        "  integer :: i\n"
        "  real :: res_dot_product\n\n"
        "  res_dot_product = 0.0\n"
        "  do i = 1, 3, 1\n"
        "    res_dot_product = res_dot_product + basis_w1(i) * wind(i)\n"
        "  enddo\n"
        "  result = res_dot_product\n\n"
        "end subroutine dot_product_test\n")
    check_trans(code, expected, fortran_reader, fortran_writer, tmpdir)
