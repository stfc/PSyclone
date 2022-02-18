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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''A module to perform pytest tests on the code in the preprocess.py
file within the psyad/transformations directory

'''
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyad.transformations.preprocess import preprocess_trans
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
    preprocess_trans(psyir)
    writer = FortranWriter()
    result = writer(psyir)
    assert result == code


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
    preprocess_trans(psyir)
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
        "      b(i) = b(i) + d(i,j) * c(j)\n"
        "    enddo\n"
        "  enddo\n"
        "  res_dot_product = 0.0\n"
        "  do i_1 = 1, 10, 1\n"
        "    res_dot_product = res_dot_product + b(i_1) * c(i_1)\n"
        "  enddo\n"
        "  a = res_dot_product\n\n"
        "end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    preprocess_trans(psyir)
    result = fortran_writer(psyir)
    assert result == expected
    assert Compile(tmpdir).string_compiles(result)


def test_preprocess_arrayrange2loop(tmpdir, fortran_reader, fortran_writer):
    '''Test that the preprocess script replaces assignments that contain
    arrays that use range notation with equivalent code that uses
    explicit loops.

    '''
    code = (
        "program test\n"
        "real, dimension(10,10,10) :: a,b,c,d\n"
        "a(:,1,:) = b(:,1,:) * c(:,1,:)\n"
        "d(1,1,1) = 0.0\n"
        "print *, \"hello\"\n"
        "end program test\n")
    expected = (
        "program test\n"
        "  real, dimension(10,10,10) :: a\n"
        "  real, dimension(10,10,10) :: b\n"
        "  real, dimension(10,10,10) :: c\n"
        "  real, dimension(10,10,10) :: d\n"
        "  integer :: idx\n"
        "  integer :: idx_1\n\n"
        "  do idx = LBOUND(a, 3), UBOUND(a, 3), 1\n"
        "    do idx_1 = LBOUND(a, 1), UBOUND(a, 1), 1\n"
        "      a(idx_1,1,idx) = b(idx_1,1,idx) * c(idx_1,1,idx)\n"
        "    enddo\n"
        "  enddo\n"
        "  d(1,1,1) = 0.0\n"
        "  PRINT *, \"hello\"\n\n"
        "end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    preprocess_trans(psyir)
    result = fortran_writer(psyir)
    print(result)
    assert result == expected
    assert Compile(tmpdir).string_compiles(result)
