# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford, STFC Daresbury Laboratory

'''Module containing tests for the sum2code transformation.'''

import pytest

from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.transformations import Sum2CodeTrans


def test_initialise():
    ''' Test that we can create an instance of the transformation '''
    trans = Sum2CodeTrans()
    assert isinstance(trans, Sum2CodeTrans)
    assert (str(trans) == "Convert the PSyIR SUM intrinsic to equivalent "
            "PSyIR code.")
    assert trans.name == "Sum2CodeTrans"

# validate tests

# apply tests

def test_apply():
    ''' Test that a sum intrinsic with a single array argument gets transformed as expected. '''
    code = (
        "program sum_test\n"
        "  real :: array(10,10)\n"
        "  real :: result\n"
        "  result = sum(array)\n"
        "end program\n")
    expected = (
        "program sum_test\n"
        "  real, dimension(10,10) :: array\n"
        "  real :: result\n"
        "  real :: sum_var\n"
        "  integer :: i_0\n"
        "  integer :: i_1\n\n"
        "  sum_var = 0.0\n"
        "  do i_1 = 1, 1, 1\n"
        "    do i_0 = 1, 1, 1\n"
        "      sum_var = sum_var + array(i_0,i_1)\n"
        "    enddo\n"
        "  enddo\n"
        "  result = sum_var\n\n"
        "end program sum_test\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/UnaryOperation
    sum_node = psyir.children[0].children[0].children[1]
    trans = Sum2CodeTrans()
    trans.apply(sum_node)
    writer = FortranWriter()
    result = writer(psyir)
    assert result == expected


# array(:,:)
# array(2:3,n:x)
# integer array
# precision


def test_args():
    ''' xxx '''
    code = (
        "program sum_test\n"
        "  real :: array(10,10)\n"
        "  real :: result\n"
        "  integer, parameter :: dimension=2\n"
        "  result = sum(array,dimension)\n"
        "end program\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/UnaryOperation
    sum_node = psyir.children[0].children[0].children[1]
    trans = Sum2CodeTrans()
    trans.apply(sum_node)
    writer = FortranWriter()
    result = writer(psyir)
    print(result)
    exit(1)


def create_matmul():
    '''Utility function that creates a valid matmul node for use with
    subsequent tests.

    '''
    symbol_table = SymbolTable()
    one = Literal("1", INTEGER_TYPE)
    two = Literal("2", INTEGER_TYPE)
    index = DataSymbol("idx", INTEGER_TYPE, constant_value=3)
    symbol_table.add(index)
    array_type = ArrayType(REAL_TYPE, [5, 10, 15])
    mat_symbol = DataSymbol("x", array_type)
    symbol_table.add(mat_symbol)
    lbound1 = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, Reference(mat_symbol), one.copy())
    ubound1 = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, Reference(mat_symbol), one.copy())
    my_mat_range1 = Range.create(lbound1, ubound1, one.copy())
    lbound2 = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, Reference(mat_symbol), two.copy())
    ubound2 = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, Reference(mat_symbol), two.copy())
    my_mat_range2 = Range.create(lbound2, ubound2, one.copy())
    matrix = ArrayReference.create(mat_symbol, [my_mat_range1, my_mat_range2,
                                                Reference(index)])
    array_type = ArrayType(REAL_TYPE, [10, 20, 10])
    vec_symbol = DataSymbol("y", array_type)
    symbol_table.add(vec_symbol)
    lbound = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, Reference(vec_symbol), one.copy())
    ubound = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, Reference(vec_symbol), one.copy())
    my_vec_range = Range.create(lbound, ubound, one.copy())
    vector = ArrayReference.create(vec_symbol, [my_vec_range,
                                                Reference(index), one.copy()])
    matmul = BinaryOperation.create(
        BinaryOperation.Operator.MATMUL, matrix, vector)
    lhs_type = ArrayType(REAL_TYPE, [10])
    lhs_symbol = DataSymbol("result", lhs_type)
    symbol_table.add(lhs_symbol)
    lhs = Reference(lhs_symbol)
    assign = Assignment.create(lhs, matmul)
    KernelSchedule.create("my_kern", symbol_table, [assign])
    return matmul
    # assert Compile(tmpdir).string_compiles(out)
