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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified: J. Henrichs, Bureau of Meteorology

'''A module to perform pytest tests on the code in the preprocess.py
file within the psyad/transformations directory

'''
import pytest

from psyclone.psyad.transformations.preprocess import preprocess_trans
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.tests.utilities import Compile


def test_preprocess_no_change():
    '''Test that the preprocess script does not modify the PSyIR if the
    script's transformations are not applicable to its contents.

    '''
    code = (
        "program test\n"
        "  integer :: a\n\n"
        "  a = 0.0\n\n"
        "end program test\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    preprocess_trans(psyir, [])
    writer = FortranWriter()
    result = writer(psyir)
    assert result == code


def test_preprocess_reference2arrayrange(tmpdir, fortran_reader,
                                         fortran_writer):
    '''Test that the preprocess script replaces assignments that contain
    arrays that use array notation with arrays using range notation
    (for example, in Fortran, from a = b * c to a(:) = b(:) * c(:)) or
    with equivalent code that uses explicit loops. Also test that
    arrays in LBOUND and UBOUND intrinsics do not get modified.

    '''
    code = (
        "program test\n"
        "real, dimension(10,10) :: a,b,c,e,f\n"
        "real, dimension(10) :: d\n"
        "integer :: i\n"
        "a = b * c\n"
        "do i = lbound(d,1), ubound(d,1)\n"
        "  d(i) = 0.0\n"
        "end do\n"
        "e = f\n"
        "end program test\n")
    expected = (
        "program test\n"
        "  real, dimension(10,10) :: a\n"
        "  real, dimension(10,10) :: b\n"
        "  real, dimension(10,10) :: c\n"
        "  real, dimension(10,10) :: e\n"
        "  real, dimension(10,10) :: f\n"
        "  real, dimension(10) :: d\n"
        "  integer :: i\n"
        "  integer :: idx\n"
        "  integer :: idx_1\n\n"
        "  do idx = 1, 10, 1\n"
        "    do idx_1 = 1, 10, 1\n"
        "      a(idx_1,idx) = b(idx_1,idx) * c(idx_1,idx)\n"
        "    enddo\n"
        "  enddo\n"
        "  do i = LBOUND(d, 1), UBOUND(d, 1), 1\n"
        "    d(i) = 0.0\n"
        "  enddo\n"
        "  e(:,:) = f(:,:)\n\n"
        "end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    preprocess_trans(psyir, ["a", "c"])
    result = fortran_writer(psyir)
    assert result == expected
    assert Compile(tmpdir).string_compiles(result)


def test_preprocess_dotproduct(tmpdir, fortran_reader, fortran_writer):
    '''Test that the preprocess script replaces a dotproduct with
    equivalent code.

    '''
    code = (
        "program test\n"
        "real :: a, b(10), c(10)\n"
        "a = dot_product(b(:), c(:))\n"
        "end program test\n")
    expected = (
        "program test\n"
        "  real :: a\n"
        "  real, dimension(10) :: b\n"
        "  real, dimension(10) :: c\n"
        "  integer :: i\n"
        "  real :: res_dot_product\n\n"
        "  res_dot_product = 0.0\n"
        "  do i = 1, 10, 1\n"
        "    res_dot_product = res_dot_product + b(i) * c(i)\n"
        "  enddo\n"
        "  a = res_dot_product\n\n"
        "end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    preprocess_trans(psyir, [])
    result = fortran_writer(psyir)
    assert result == expected
    assert Compile(tmpdir).string_compiles(result)


def test_preprocess_matmul(tmpdir, fortran_reader, fortran_writer):
    '''Test that the preprocess script replaces a matmul with equivalent
    code. Mix with a dot_product to make sure both get transformed.

    '''
    code = (
        "program test\n"
        "real :: a, b(10), c(10), d(10,10)\n"
        "b = matmul(d,c)\n"
        "a = dot_product(b(:), c(:))\n"
        "end program test\n")
    expected = (
        "program test\n"
        "  real :: a\n"
        "  real, dimension(10) :: b\n"
        "  real, dimension(10) :: c\n"
        "  real, dimension(10,10) :: d\n"
        "  integer :: i\n"
        "  integer :: j\n"
        "  integer :: i_1\n"
        "  real :: res_dot_product\n\n"
        "  do i = 1, 10, 1\n"
        "    b(i) = 0.0\n"
        "    do j = 1, 10, 1\n"
        "      b(i) = b(i) + c(j) * d(i,j)\n"
        "    enddo\n"
        "  enddo\n"
        "  res_dot_product = 0.0\n"
        "  do i_1 = 1, 10, 1\n"
        "    res_dot_product = res_dot_product + b(i_1) * c(i_1)\n"
        "  enddo\n"
        "  a = res_dot_product\n\n"
        "end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    preprocess_trans(psyir, [])
    result = fortran_writer(psyir)
    assert result == expected
    assert Compile(tmpdir).string_compiles(result)


def test_preprocess_arrayrange2loop(tmpdir, fortran_reader, fortran_writer):
    '''Test that the preprocess script replaces active assignments that
    contain arrays that use range notation with equivalent code that
    uses explicit loops. Also check that they are not modified if they
    are not active.

    '''
    code = (
        "program test\n"
        "real, dimension(10,10,10) :: a,b,c,d,e,f\n"
        "a(:,1,:) = b(:,1,:) * c(:,1,:)\n"
        "d(1,1,1) = 0.0\n"
        "e(:,:,:) = f(:,:,:)\n"
        "print *, \"hello\"\n"
        "end program test\n")
    expected = (
        "program test\n"
        "  real, dimension(10,10,10) :: a\n"
        "  real, dimension(10,10,10) :: b\n"
        "  real, dimension(10,10,10) :: c\n"
        "  real, dimension(10,10,10) :: d\n"
        "  real, dimension(10,10,10) :: e\n"
        "  real, dimension(10,10,10) :: f\n"
        "  integer :: idx\n"
        "  integer :: idx_1\n\n"
        "  do idx = LBOUND(a, dim=3), UBOUND(a, dim=3), 1\n"
        "    do idx_1 = LBOUND(a, dim=1), UBOUND(a, dim=1), 1\n"
        "      a(idx_1,1,idx) = b(idx_1,1,idx) * c(idx_1,1,idx)\n"
        "    enddo\n"
        "  enddo\n"
        "  d(1,1,1) = 0.0\n"
        "  e(:,:,:) = f(:,:,:)\n"
        "  PRINT *, \"hello\"\n\n"
        "end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    preprocess_trans(psyir, ["a", "c"])
    result = fortran_writer(psyir)
    assert result == expected
    assert Compile(tmpdir).string_compiles(result)


@pytest.mark.parametrize("operation", ["+", "-"])
def test_preprocess_associativity(operation, fortran_reader, fortran_writer):
    '''Test that associativity is handled correctly.

    '''
    code = (
        f"program test\n"
        f"  integer :: a, b, c, d\n"
        f"  a = b * (c {operation} d)\n"
        f"end program test\n")
    expected = (
        f"program test\n"
        f"  integer :: a\n"
        f"  integer :: b\n"
        f"  integer :: c\n"
        f"  integer :: d\n\n"
        f"  a = b * c {operation} b * d\n\n"
        f"end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    preprocess_trans(psyir, ["a", "c", "d"])
    result = fortran_writer(psyir)
    assert result == expected


@pytest.mark.parametrize("operation", ["*", "/"])
def test_associativity2(operation, fortran_reader, fortran_writer):
    '''Test that associativity  works as expected for (a+b)*/x
    where a and b are active and x is inactive.

    '''
    code = (
        f"program test\n"
        f"  integer :: a, b, c, d, e, f\n"
        f"  a = (c + d) {operation} (b*e/f)\n"
        f"end program test\n")
    if operation == "*":
        expr = "b * c * e / f + b * d * e / f"
    else:
        expr = "c * f / (b * e) + d * f / (b * e)"
    expected = (
        f"program test\n"
        f"  integer :: a\n"
        f"  integer :: b\n"
        f"  integer :: c\n"
        f"  integer :: d\n"
        f"  integer :: e\n"
        f"  integer :: f\n\n"
        f"  a = {expr}\n\n"
        f"end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    preprocess_trans(psyir, ["a", "c", "d"])
    result = fortran_writer(psyir)
    assert result == expected


def test_associativity3(fortran_reader, fortran_writer):
    '''Test that associativity works as expected when it
    needs to be applied multiple times in an assignment.

    '''
    code = (
        "program test\n"
        "  integer :: a, b, c, d, e, f, g\n"
        "  a = b * (c + f * (d + g)) / e\n"
        "end program test\n")
    expected = (
        "program test\n"
        "  integer :: a\n"
        "  integer :: b\n"
        "  integer :: c\n"
        "  integer :: d\n"
        "  integer :: e\n"
        "  integer :: f\n"
        "  integer :: g\n\n"
        "  a = b * c / e + b * d * f / e + b * f * g / e\n\n"
        "end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    preprocess_trans(psyir.children[0][0], ["d", "c", "g"])
    result = fortran_writer(psyir)
    assert result == expected


def test_preprocess_associativity4(fortran_reader, fortran_writer):
    '''Test that associativity works as expected when we have
    array ranges.

    '''
    code = (
        "program test\n"
        "  integer :: a, b, c(10), d(10)\n"
        "  a = b*(sum(c(:)) + sum(d(:)))\n"
        "end program test\n")
    expected = (
        "program test\n"
        "  integer :: a\n"
        "  integer :: b\n"
        "  integer, dimension(10) :: c\n"
        "  integer, dimension(10) :: d\n\n"
        "  a = b * SUM(c(:)) + b * SUM(d(:))\n\n"
        "end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    preprocess_trans(psyir, ["a", "c", "d"])
    result = fortran_writer(psyir)
    assert result == expected


def test_associativity5(tmpdir, fortran_reader, fortran_writer):
    '''Test that associativity works as expected when we have
    a literal as part of the expression that we would like to
    expand.

    '''
    code = (
        "subroutine example(a,b,c)\n"
        "  real :: a,b,c\n"
        "  a = 0.5*(b + c)\n"
        "end subroutine\n")
    expected = (
        "subroutine example(a, b, c)\n"
        "  real :: a\n"
        "  real :: b\n"
        "  real :: c\n\n"
        "  a = 0.5 * b + 0.5 * c\n\n"
        "end subroutine example\n")
    psyir = fortran_reader.psyir_from_source(code)
    preprocess_trans(psyir, ["a", "b", "c"])
    result = fortran_writer(psyir)
    assert result == expected
    assert Compile(tmpdir).string_compiles(result)


def test_associativity6(tmpdir, fortran_reader, fortran_writer):
    '''Test that associativity works as expected when we have a negative
    literal as part of the expression that we would like to expand.

    '''
    code = (
        "subroutine example(a,b,c)\n"
        "  real :: a,b,c\n"
        "  a = -0.5*(b + c)\n"
        "end subroutine\n")
    expected = (
        "subroutine example(a, b, c)\n"
        "  real :: a\n"
        "  real :: b\n"
        "  real :: c\n\n"
        "  a = -0.5 * b - 0.5 * c\n\n"
        "end subroutine example\n")
    psyir = fortran_reader.psyir_from_source(code)
    preprocess_trans(psyir, ["a", "b", "c"])
    result = fortran_writer(psyir)
    assert result == expected
    assert Compile(tmpdir).string_compiles(result)


def test_associativity7(fortran_reader, fortran_writer):
    '''Test that associativity works as expected when we have
    user-defined types.
    '''
    code = (
        "program test\n"
        "  use my_type_mod\n"
        "  integer :: a, b, c(10), d(10)\n"
        "  type(my_type) :: mt\n"
        "  a = b*(sum(c(:)) + sum(mt%x(:)))\n"
        "end program test\n")
    expected = (
        "program test\n"
        "  use my_type_mod\n"
        "  integer :: a\n"
        "  integer :: b\n"
        "  integer, dimension(10) :: c\n"
        "  integer, dimension(10) :: d\n"
        "  type(my_type) :: mt\n\n"
        "  a = b * SUM(c(:)) + b * SUM(mt%x(:))\n\n"
        "end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    preprocess_trans(psyir, ["a", "c", "mt"])
    result = fortran_writer(psyir)
    assert result == expected
