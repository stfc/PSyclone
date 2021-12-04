# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council
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
from psyclone.psyir.transformations.intrinsics.dotproduct2code_trans import \
    DotProduct2CodeTrans
from psyclone.tests.utilities import Compile

# bounds [TBD]

def test_initialise():
    '''Check that the class DotProduct2CodeTrans behaves as expected when
    an instance of the class is created.

    '''
    trans = DotProduct2CodeTrans()
    assert trans._operator_name == "DOTPRODUCT"
    assert (str(trans) == "Convert the PSyIR DOTPRODUCT intrinsic to "
            "equivalent PSyIR code.")
    assert trans.name == "Dotproduct2CodeTrans"

# validate [TBD]

# apply
@pytest.mark.parametrize("dim1,dim2", [("10", "10"), (":", "10"), ("10", ":")])
def test_apply1(tmpdir, fortran_reader, fortran_writer, dim1, dim2):
    '''Test that the dotproduct2code apply method produces the expected
    PSyIR when at least one of the vectors has a known dimension.

    '''
    code = (
        f"subroutine dot_product_test(v1,v2)\n"
        f"real,intent(in) :: v1({dim1}), v2({dim2})\n"
        f"real :: result\n"
        f"result = dot_product(v1,v2)\n"
        f"end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    dot_product = psyir.walk(BinaryOperation)[0]
    assert dot_product.operator == BinaryOperation.Operator.DOT_PRODUCT

    trans = DotProduct2CodeTrans()
    trans.apply(dot_product)
    result = fortran_writer(psyir)
    expected = (
        "  integer :: i\n"
        "  real :: res_dot_product\n\n"
        "  res_dot_product = 0.0\n"
        "  do i = 1, 10, 1\n"
        "    res_dot_product = res_dot_product + v1(i) * v2(i)\n"
        "  enddo\n"
        "  result = res_dot_product\n\n")
    assert (expected in result)
    assert Compile(tmpdir).string_compiles(result)


def test_apply2(tmpdir, fortran_reader, fortran_writer):
    '''Test that the dotproduct2code apply method produces the expected
    PSyIR when neither of the vectors have a known size.

    '''
    code = (
        "subroutine dot_product_test(v1,v2)\n"
        "real,intent(in) :: v1(:), v2(:)\n"
        "real :: result\n"
        "result = dot_product(v1,v2)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    dot_product = psyir.walk(BinaryOperation)[0]
    assert dot_product.operator == BinaryOperation.Operator.DOT_PRODUCT

    trans = DotProduct2CodeTrans()
    trans.apply(dot_product)
    result = fortran_writer(psyir)
    expected = (
        "  integer :: i\n"
        "  real :: res_dot_product\n\n"
        "  res_dot_product = 0.0\n"
        "  do i = LBOUND(v1, 1), UBOUND(v1, 1), 1\n"
        "    res_dot_product = res_dot_product + v1(i) * v2(i)\n"
        "  enddo\n"
        "  result = res_dot_product\n\n")
    print (result)
    assert (expected in result)
    assert Compile(tmpdir).string_compiles(result)

# multiple stuff on rhs
# use of array notation on rhs
# extra dimensions (not vector)
# not first dimension iterating over
# array slices
