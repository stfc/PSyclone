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
from psyclone.psyir.nodes import BinaryOperation, Literal
from psyclone.psyir.symbols import REAL_TYPE
from psyclone.psyir.transformations import Sum2CodeTrans, TransformationError


def test_initialise():
    ''' Test that we can create an instance of the transformation '''
    trans = Sum2CodeTrans()
    assert isinstance(trans, Sum2CodeTrans)
    assert (str(trans) == "Convert the PSyIR SUM intrinsic to equivalent "
            "PSyIR code.")
    assert trans.name == "Sum2CodeTrans"


def test_validate_node():
    '''Check that an incorrect node raises the expected exception.'''
    trans = Sum2CodeTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert ("Error in Sum2CodeTrans transformation. The supplied node "
            "argument is not a SUM operator, found 'NoneType'."
            in str(info.value))

    bin_op = BinaryOperation.create(
        BinaryOperation.Operator.MUL, Literal("1.0", REAL_TYPE),
        Literal("1.0", REAL_TYPE))
    with pytest.raises(TransformationError) as info:
        trans.validate(bin_op)
    assert ("Error in Sum2CodeTrans transformation. The supplied node "
            "operator is invalid, found 'Operator.MUL'." in str(info.value))


# Literal value of dimension argument
def test_dimension_arg(fortran_reader):
    '''Test that the expected exception is raised if the dimension arg is
    not a literal or a variable.

    '''
    code = (
        "subroutine sum_test(array,n,m)\n"
        "  integer :: n, m\n"
        "  real :: array(10,10)\n"
        "  real :: result\n"
        "  integer :: dimension\n"
        "  result = sum(array,dimension*2)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/UnaryOperation
    sum_node = psyir.children[0].children[0].children[1]
    trans = Sum2CodeTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(sum_node)
    assert ("Can't find the value of the dimension argument. Expected it to "
            "be a literal or a reference but found 'dimension * 2' which is "
            "a 'BinaryOperation'." in str(info.value))


def test_array_arg(fortran_reader):
    '''Test that the expected exception is raised if the array argument is
    not an array.

    '''
    code = (
        "subroutine sum_test(array,n,m)\n"
        "  integer :: n, m\n"
        "  real :: array\n"
        "  real :: result\n"
        "  result = sum(array)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/UnaryOperation
    sum_node = psyir.children[0].children[0].children[1]
    trans = Sum2CodeTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(sum_node)
    assert "Expected 'array' to be an array." in str(info.value)


def test_array_shape(fortran_reader, monkeypatch):
    '''Tests that the expected exception is raised if the array shape is
    not a valid value. Requires monkeypatching.

    '''
    code = (
        "subroutine sum_test(array,n,m)\n"
        "  integer :: n, m\n"
        "  real :: array(1)\n"
        "  real :: result\n"
        "  result = sum(array)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/UnaryOperation
    sum_node = psyir.children[0].children[0].children[1]

    # Modify array shape from sum_node to create exception
    array_ref = sum_node.children[0]
    array_symbol = array_ref.symbol
    monkeypatch.setattr(array_symbol._datatype, "_shape", [None])

    trans = Sum2CodeTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(sum_node)
    assert ("Unexpected shape for array. Expecting one of Deferred, "
            "Attribute or Bounds but found 'None'." in str(info.value))


def test_array_type_arg(fortran_reader):
    '''Test that the expected exception is raised if the array is an
    unsupported datatype.

    '''
    code = (
        "subroutine sum_test(array,n,m)\n"
        "  integer :: n, m\n"
        "  logical :: array(10)\n"
        "  real :: result\n"
        "  result = sum(array)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/UnaryOperation
    sum_node = psyir.children[0].children[0].children[1]
    trans = Sum2CodeTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(sum_node)
    assert ("Only real and integer types supported for array 'array', "
            "but found 'BOOLEAN'." in str(info.value))


# apply tests

@pytest.mark.parametrize("idim1,idim2,rdim11,rdim12,rdim21,rdim22",
                         [("10", "20", "1", "10", "1", "20"),
                          ("n", "m", "1", "n", "1", "m"),
                          ("0:n", "2:m", "0", "n", "2", "m"),
                          (":", ":", "LBOUND(array, 1)", "UBOUND(array, 1)",
                           "LBOUND(array, 2)", "UBOUND(array, 2)")])
def test_apply_sum(idim1, idim2, rdim11, rdim12, rdim21, rdim22,
                   fortran_reader, fortran_writer):
    '''Test that a sum intrinsic as the only term on the rhs of an
    assignment with a single array argument gets transformed as
    expected. Test with known and unknown array sizes.

    '''
    code = (
        f"subroutine sum_test(array,n,m)\n"
        f"  integer :: n, m\n"
        f"  real :: array({idim1},{idim2})\n"
        f"  real :: result\n"
        f"  result = sum(array)\n"
        f"end subroutine\n")
    expected = (
        f"subroutine sum_test(array, n, m)\n"
        f"  integer :: n\n  integer :: m\n"
        f"  real, dimension({idim1},{idim2}) :: array\n"
        f"  real :: result\n  real :: sum_var\n"
        f"  integer :: i_0\n  integer :: i_1\n\n"
        f"  sum_var = 0.0\n"
        f"  do i_1 = {rdim21}, {rdim22}, 1\n"
        f"    do i_0 = {rdim11}, {rdim12}, 1\n"
        f"      sum_var = sum_var + array(i_0,i_1)\n"
        f"    enddo\n"
        f"  enddo\n"
        f"  result = sum_var\n\n"
        f"end subroutine sum_test\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/UnaryOperation
    sum_node = psyir.children[0].children[0].children[1]
    trans = Sum2CodeTrans()
    trans.apply(sum_node)
    result = fortran_writer(psyir)
    assert result == expected


@pytest.mark.parametrize("datatype,zero", [("real", "0.0"), ("integer", "0"),
                                           ("real(kind=r_def)", "0.0_r_def")])
def test_apply_sum_multi(fortran_reader, fortran_writer, datatype, zero):
    '''Test that a sum intrinsic as part of multiple term on the rhs of an
    assignment with a single array argument gets transformed as
    expected. Test with real, integer and with a specified precision.

    '''
    code = (
        f"subroutine sum_test(array,n,m,value1,value2)\n"
        f"  use precision\n"
        f"  integer :: n, m\n"
        f"  {datatype} :: array(n,m)\n"
        f"  real :: value1, value2\n"
        f"  real :: result\n"
        f"  result = value1 + sum(array) * value2\n"
        f"end subroutine\n")
    expected = (
        f"subroutine sum_test(array, n, m, value1, value2)\n"
        f"  use precision\n"
        f"  integer :: n\n  integer :: m\n"
        f"  {datatype}, dimension(n,m) :: array\n"
        f"  real :: value1\n  real :: value2\n"
        f"  real :: result\n  {datatype} :: sum_var\n"
        f"  integer :: i_0\n  integer :: i_1\n\n"
        f"  sum_var = {zero}\n"
        f"  do i_1 = 1, m, 1\n"
        f"    do i_0 = 1, n, 1\n"
        f"      sum_var = sum_var + array(i_0,i_1)\n"
        f"    enddo\n"
        f"  enddo\n"
        f"  result = value1 + sum_var * value2\n\n"
        f"end subroutine sum_test\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/BinaryOperation(ADD)/
    # BinaryOperation(MUL)/UnaryOperation
    sum_node = psyir.children[0].children[0].children[1].children[1]. \
        children[0]
    trans = Sum2CodeTrans()
    trans.apply(sum_node)
    result = fortran_writer(psyir)
    assert result == expected


def test_apply_dimension_1d(fortran_reader, fortran_writer):
    '''Test that the apply method works as expected when a dimension
    argument is specified and the array is one dimensional. This
    should be the same as if dimension were not specified at all.

    '''
    code = (
        "subroutine sum_test(array,value1,value2)\n"
        "  real :: array(:)\n"
        "  real :: value1, value2\n"
        "  real :: result\n"
        "  result = value1 + sum(array,dimension=1) * value2\n"
        "end subroutine\n")
    expected = (
        "subroutine sum_test(array, value1, value2)\n"
        "  real, dimension(:) :: array\n"
        "  real :: value1\n  real :: value2\n"
        "  real :: result\n  real :: sum_var\n"
        "  integer :: i_0\n\n"
        "  sum_var = 0.0\n"
        "  do i_0 = LBOUND(array, 1), UBOUND(array, 1), 1\n"
        "    sum_var = sum_var + array(i_0)\n"
        "  enddo\n"
        "  result = value1 + sum_var * value2\n\n"
        "end subroutine sum_test\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/BinaryOperation(ADD)/
    # BinaryOperation(MUL)/UnaryOperation
    sum_node = psyir.children[0].children[0].children[1].children[1]. \
        children[0]
    trans = Sum2CodeTrans()
    trans.apply(sum_node)
    result = fortran_writer(psyir)
    assert result == expected


def test_apply_dimension_multid(fortran_reader, fortran_writer):
    '''Test that the apply method works as expected when a dimension
    argument is specified and the array is multi-dimensional. Only the
    specified dimension should be summed.

    '''
    code = (
        "subroutine sum_test(array,value1,value2,n,m,p)\n"
        "  integer :: n,m,p\n"
        "  real :: array(n,m,p)\n"
        "  real :: value1, value2\n"
        "  real :: result(n,p)\n"
        "  result(:,:) = value1 + sum(array,dimension=2) * value2\n"
        "end subroutine\n")
    expected = (
        "subroutine sum_test(array, value1, value2, n, m, p)\n"
        "  integer :: n\n  integer :: m\n  integer :: p\n"
        "  real, dimension(n,m,p) :: array\n"
        "  real :: value1\n  real :: value2\n"
        "  real, dimension(n,p) :: result\n"
        "  real, dimension(n,p) :: sum_var\n"
        "  integer :: i_0\n  integer :: i_1\n  integer :: i_2\n\n"
        "  sum_var = 0.0\n"
        "  do i_2 = 1, p, 1\n"
        "    do i_1 = 1, m, 1\n"
        "      do i_0 = 1, n, 1\n"
        "        sum_var(i_0,i_2) = sum_var(i_0,i_2) + array(i_0,i_1,i_2)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n"
        "  result(:,:) = value1 + sum_var * value2\n\n"
        "end subroutine sum_test\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/BinaryOperation(ADD)/
    # BinaryOperation(MUL)/UnaryOperation
    sum_node = psyir.children[0].children[0].children[1].children[1]. \
        children[0]
    trans = Sum2CodeTrans()
    trans.apply(sum_node)
    result = fortran_writer(psyir)
    assert result == expected


def test_apply_dimension_multid_unknown(fortran_reader, fortran_writer):
    '''Test that lbound and ubound are used if the bounds of the array are
    not known.

    '''
    code = (
        "subroutine sum_test(array,value1,value2,result)\n"
        "  real :: array(:,:,:)\n"
        "  real :: value1, value2\n"
        "  real :: result(:,:)\n"
        "  result(:,:) = value1 + sum(array,dimension=2) * value2\n"
        "end subroutine\n")
    expected = (
        "subroutine sum_test(array, value1, value2, result)\n"
        "  real, dimension(:,:,:) :: array\n"
        "  real :: value1\n"
        "  real :: value2\n"
        "  real, dimension(:,:) :: result\n"
        "  real, dimension(LBOUND(array, 1):UBOUND(array, 1),LBOUND(array, 3):"
        "UBOUND(array, 3)) :: sum_var\n"
        "  integer :: i_0\n"
        "  integer :: i_1\n"
        "  integer :: i_2\n\n"
        "  sum_var = 0.0\n"
        "  do i_2 = LBOUND(array, 3), UBOUND(array, 3), 1\n"
        "    do i_1 = LBOUND(array, 2), UBOUND(array, 2), 1\n"
        "      do i_0 = LBOUND(array, 1), UBOUND(array, 1), 1\n"
        "        sum_var(i_0,i_2) = sum_var(i_0,i_2) + array(i_0,i_1,i_2)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n"
        "  result(:,:) = value1 + sum_var * value2\n\n"
        "end subroutine sum_test\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/BinaryOperation(ADD)/
    # BinaryOperation(MUL)/UnaryOperation
    sum_node = psyir.children[0].children[0].children[1].children[1]. \
        children[0]
    trans = Sum2CodeTrans()
    trans.apply(sum_node)
    result = fortran_writer(psyir)
    assert result == expected


# specified array range
def test_apply_dimension_multid_range(fortran_reader, fortran_writer):
    '''Test that the apply method works as expected when an array range is
    specified and the array is multi-dimensional. Only the specified
    dimension should be summed.

    '''
    code = (
        "subroutine sum_test(array,value1,value2,n,m,p)\n"
        "  integer :: n,m,p\n"
        "  real :: array(:,:,:)\n"
        "  real :: value1, value2\n"
        "  real :: result(n,p)\n"
        "  result(:,:) = value1 + sum(array(1:n,m-1:m,1:p),dimension=2) * "
        "value2\n"
        "end subroutine\n")
    expected = (
        "subroutine sum_test(array, value1, value2, n, m, p)\n"
        "  integer :: n\n  integer :: m\n  integer :: p\n"
        "  real, dimension(:,:,:) :: array\n"
        "  real :: value1\n  real :: value2\n"
        "  real, dimension(n,p) :: result\n"
        "  real, dimension(n,p) :: sum_var\n"
        "  integer :: i_0\n  integer :: i_1\n  integer :: i_2\n\n"
        "  sum_var = 0.0\n"
        "  do i_2 = 1, p, 1\n"
        "    do i_1 = m - 1, m, 1\n"
        "      do i_0 = 1, n, 1\n"
        "        sum_var(i_0,i_2) = sum_var(i_0,i_2) + array(i_0,i_1,i_2)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n"
        "  result(:,:) = value1 + sum_var * value2\n\n"
        "end subroutine sum_test\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/BinaryOperation(ADD)/
    # BinaryOperation(MUL)/UnaryOperation
    sum_node = psyir.children[0].children[0].children[1].children[1]. \
        children[0]
    trans = Sum2CodeTrans()
    trans.apply(sum_node)
    result = fortran_writer(psyir)
    assert result == expected


def test_mask():
    '''Test that the sum transformation works when there is a mask
    specified.

    '''
    code = (
        "program sum_test\n"
        "  real :: array(10,10)\n"
        "  real :: result\n"
        "  result = sum(array, mask=MOD(array, 2.0)==1)\n"
        "end program\n")
    expected = (
        "program sum_test\n"
        "  real, dimension(10,10) :: array\n"
        "  real :: result\n"
        "  real :: sum_var\n"
        "  integer :: i_0\n"
        "  integer :: i_1\n\n"
        "  sum_var = 0.0\n"
        "  do i_1 = 1, 10, 1\n"
        "    do i_0 = 1, 10, 1\n"
        "      if (MOD(array(i_0,i_1), 2.0) == 1) then\n"
        "        sum_var = sum_var + array(i_0,i_1)\n"
        "      end if\n"
        "    enddo\n"
        "  enddo\n"
        "  result = sum_var\n\n"
        "end program sum_test")
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/UnaryOperation
    sum_node = psyir.children[0].children[0].children[1]
    trans = Sum2CodeTrans()
    trans.apply(sum_node)
    writer = FortranWriter()
    result = writer(psyir)
    assert expected in result


def test_mask_dimension():
    '''Test that the sum transformation works when there is a mask and a
    dimension specified.

    '''
    code = (
        "program sum_test\n"
        "  real :: array(10,10)\n"
        "  real :: result(10)\n"
        "  integer, parameter :: dimension=2\n"
        "  result = sum(array, dimension, mask=MOD(array, 2.0)==1)\n"
        "end program\n")
    expected = (
        "program sum_test\n"
        "  integer, parameter :: dimension = 2\n"
        "  real, dimension(10,10) :: array\n"
        "  real, dimension(10) :: result\n"
        "  real, dimension(10) :: sum_var\n"
        "  integer :: i_0\n"
        "  integer :: i_1\n\n"
        "  sum_var = 0.0\n"
        "  do i_1 = 1, 10, 1\n"
        "    do i_0 = 1, 10, 1\n"
        "      if (MOD(array(i_0,i_1), 2.0) == 1) then\n"
        "        sum_var(i_0) = sum_var(i_0) + array(i_0,i_1)\n"
        "      end if\n"
        "    enddo\n"
        "  enddo\n"
        "  result = sum_var\n\n"
        "end program sum_test")
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/UnaryOperation
    sum_node = psyir.children[0].children[0].children[1]
    trans = Sum2CodeTrans()
    trans.apply(sum_node)
    writer = FortranWriter()
    result = writer(psyir)
    assert expected in result


# todo var with : notation?
