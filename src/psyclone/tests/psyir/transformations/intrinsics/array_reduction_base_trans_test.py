# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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

'''Module containing tests for the array_reduction_base_trans which is
an abstract parent class for the array-reduction intrinsic
transformations.

'''
import pytest

from psyclone.psyir.nodes import IntrinsicCall, Reference, Literal
from psyclone.psyir.symbols import (
    Symbol, BOOLEAN_TYPE, INTEGER_TYPE, DataSymbol, REAL_TYPE)
from psyclone.psyir.transformations import (
    TransformationError, Maxval2LoopTrans)
from psyclone.psyir.transformations.intrinsics.array_reduction_base_trans \
    import ArrayReductionBaseTrans
from psyclone.tests.utilities import Compile


def test_init_exception():
    '''Check that this class can't be created as it is abstract.'''
    # pylint: disable=abstract-class-instantiated
    with pytest.raises(TypeError) as info:
        _ = ArrayReductionBaseTrans()
    # Python >= 3.12 tweaks the error message to mention
    # the lack of an implementation and to quote the method names.
    # We split the check to accomodate for this.
    assert ("Can't instantiate abstract class ArrayReductionBaseTrans with"
            in str(info.value))
    assert "abstract methods" in str(info.value)
    assert "_init_var" in str(info.value)
    assert "_loop_body" in str(info.value)


def test_get_args():
    '''Check the _get_args static method works as expected.'''
    # array
    array_reference = Reference(Symbol("array"))
    node = IntrinsicCall.create(IntrinsicCall.Intrinsic.SUM, [array_reference])
    result = ArrayReductionBaseTrans._get_args(node)
    assert result == (array_reference, None, None)

    # array, mask, dim
    mask_reference = Literal("true", BOOLEAN_TYPE)
    dim_reference = Literal("1", INTEGER_TYPE)
    node = IntrinsicCall.create(IntrinsicCall.Intrinsic.SUM, [
        array_reference.copy(), ("mask", mask_reference),
        ("dim", dim_reference)])
    result = ArrayReductionBaseTrans._get_args(node)
    assert result == (array_reference, dim_reference, mask_reference)


def test_str():
    ''' Check that the __str__ method behaves as expected. '''
    assert str(Maxval2LoopTrans()) == ("Convert the PSyIR MAXVAL intrinsic to "
                                       "equivalent PSyIR code.")


# validate method

def test_validate_node():
    '''Check that an incorrect node raises the expected exception.'''
    trans = Maxval2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert ("Error in Maxval2LoopTrans transformation. The supplied node "
            "argument is not an intrinsic, found 'NoneType'."
            in str(info.value))

    intrinsic = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.MINVAL,
        [Reference(DataSymbol("array", REAL_TYPE))])
    with pytest.raises(TransformationError) as info:
        trans.validate(intrinsic)
    assert ("The supplied node argument is not a maxval intrinsic, found "
            "'MINVAL'." in str(info.value))


def test_structure_error(fortran_reader):
    '''Test that the transformation raises an exception if the array node
    is part of a structure and has no array references within the
    array node.

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
        "  result = maxval(ref%array)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0].children[1]
    trans = Maxval2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(node)
    assert ("Error, no ArrayReference's found in the expression 'ref%array'."
            in str(info.value))


def test_lhs(fortran_reader):
    '''Test that an exception is raised when an array-reduction intrinsic
    is on the LHS of an asssignment. Uses the Maxval2LoopTrans
    transformation (a subclass of ArrayReductionBaseTrans), as it is
    easier to test.

    '''
    code = (
        "subroutine test(array)\n"
        "  real :: array(10)\n"
        "  real :: result(10)\n"
        "  result(maxval(array)) = 0.0\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Maxval2LoopTrans()
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.children[0].children[0].children[0].children[0]
    with pytest.raises(TransformationError) as info:
        trans.validate(node)
    assert ("Error, intrinsics on the lhs of an assignment are not currently "
            "supported." in str(info.value))


def test_array_shape(fortran_reader, monkeypatch):
    '''Tests that the expected exception is raised if the array range is
    not a valid value. Requires monkeypatching.

    '''
    code = (
        "subroutine test(array,n,m)\n"
        "  integer :: n, m\n"
        "  real :: array(1)\n"
        "  real :: result\n"
        "  result = maxval(array)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.children[0].children[0].children[1]

    # Modify array shape from node to create exception
    array_ref = node.children[0]
    array_symbol = array_ref.symbol
    monkeypatch.setattr(array_symbol._datatype, "_shape", [None])

    trans = Maxval2LoopTrans()
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
        "  result = maxval(array)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.children[0].children[0].children[1]
    array_ref = node.children[0]
    # Modify the shape of the array reference shape to create an
    # exception
    monkeypatch.setattr(array_ref.symbol._datatype, "_shape", [1])

    trans = Maxval2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(node)
    assert ("Unexpected shape for array. Expecting one of Deferred, Attribute "
            "or Bounds but found '1'." in str(info.value))


def test_not_assignment(fortran_reader):
    '''Test that the expected exception is raised if the intrinsic call is
    not part of an assignment (e.g. is an argument to a subroutine),
    as this is not currently supported.

    '''
    code = (
        "subroutine test(array)\n"
        "  integer :: array(10)\n"
        "  call routine(maxval(array))\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Call/IntrinsicCall
    node = psyir.children[0].children[0].children[0]
    trans = Maxval2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(node)
    assert ("Maxval2LoopTrans only works when the intrinsic is part "
            "of an Assignment" in str(info.value))


def test_validate_increment_with_unsupported_type(fortran_reader):
    '''Check that the expected error is produced when the resulting code
    needs a temporary variable but the lhs type can not be resolved.

    '''
    code = (
        "subroutine test()\n"
        "use othermod\n"
        "real :: a(10)\n"
        "x(1) = x(1) + maxval(a)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Maxval2LoopTrans()
    node = psyir.walk(IntrinsicCall)[0]
    with pytest.raises(TransformationError) as info:
        trans.apply(node)
    assert ("To loopify 'x(1) = x(1) + MAXVAL(a)' we need a temporary "
            "variable, but the type of 'x(1)' can not be resolved or is "
            "unsupported." in str(info.value))


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
    '''Test that a maxval intrinsic as the only term on the rhs of an
    assignment with a single array argument gets transformed as
    expected. Test with known and unknown array sizes. What we care
    about here are the initialisation of the result variable, the
    generated intrinsic (MAX) and the loop bounds.

    '''
    code = (
        f"subroutine test(array,n,m)\n"
        f"  integer :: n,m\n"
        f"  real :: array({idim1},{idim2})\n"
        f"  real :: result\n"
        f"  result = maxval(array)\n"
        f"end subroutine\n")
    expected = (
        f"  result = -HUGE(result)\n"
        f"  do idx = {rdim21}, {rdim22}, 1\n"
        f"    do idx_1 = {rdim11}, {rdim12}, 1\n"
        f"      result = MAX(result, array(idx_1,idx))\n"
        f"    enddo\n"
        f"  enddo\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.walk(IntrinsicCall)[0]
    trans = Maxval2LoopTrans()
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_apply_multi(fortran_reader, fortran_writer, tmpdir):
    '''Test that a MAXVAL intrinsic as part of multiple term on the rhs of
    an assignment with a single array argument gets transformed as
    expected. What we care about here are the initialisation of the
    result variable, the generated intrinsic (MAX) and the loop
    bounds.

    '''
    code = (
        "subroutine test(array,n,m,value1,value2)\n"
        "  integer :: n, m\n"
        "  real :: array(n,m)\n"
        "  real :: value1, value2\n"
        "  real :: result\n"
        "  result = value1 + maxval(array) * value2\n"
        "end subroutine\n")
    expected = (
        "  result = -HUGE(result)\n"
        "  do idx = 1, m, 1\n"
        "    do idx_1 = 1, n, 1\n"
        "      result = MAX(result, array(idx_1,idx))\n"
        "    enddo\n"
        "  enddo\n"
        "  result = value1 + result * value2\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/BinaryOperation(ADD)/
    # BinaryOperation(MUL)/IntrinsicCall
    node = psyir.walk(IntrinsicCall)[0]
    trans = Maxval2LoopTrans()
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_apply_dimension_1d(fortran_reader):
    '''Test that the apply method works as expected when a dimension
    argument is specified and the array is one dimensional. This
    should be the same as if dimension were not specified at all.
    What we care about here are the initialisation of the result
    variable, the generated intrinsic (MAX) and the loop bounds.

    '''
    code = (
        "subroutine test(array)\n"
        "  real :: array(:)\n"
        "  real :: result\n"
        "  result = maxval(array,dim=1)\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.walk(IntrinsicCall)[0]
    trans = Maxval2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(node)
    assert ("The dimension argument to MAXVAL is not yet supported."
            in str(info.value))


def test_mask(fortran_reader, fortran_writer, tmpdir):
    '''Test that the transformation works when there is a mask specified.
    What we care about here are the initialisation of the result
    variable, the generated intrinsic (MAX) and the loop bounds.

    '''
    code = (
        "program test\n"
        "  real :: array(10,10)\n"
        "  real :: result\n"
        "  result = maxval(array, mask=MOD(array, 2.0)==1)\n"
        "end program\n")
    expected = (
        "  result = -HUGE(result)\n"
        "  do idx = 1, 10, 1\n"
        "    do idx_1 = 1, 10, 1\n"
        "      if (MOD(array(idx_1,idx), 2.0) == 1) then\n"
        "        result = MAX(result, array(idx_1,idx))\n"
        "      end if\n"
        "    enddo\n"
        "  enddo\n")
    psyir = fortran_reader.psyir_from_source(code)
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.walk(IntrinsicCall)[0]
    trans = Maxval2LoopTrans()
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_mask_array_indexed(fortran_reader, fortran_writer, tmpdir):
    '''Test that the mask code works if the array iself it used as part of
    the mask. In this case it will already be indexed. Use the
    Maxval2LoopTrans transformation (a subclass of
    ArrayReductionBaseTrans), as it is easier to test.

    '''
    code = (
        "program sum_test\n"
        "  real :: a(4)\n"
        "  real :: result\n"
        "  a(1) = 2.0\n"
        "  a(2) = 1.0\n"
        "  a(3) = 2.0\n"
        "  a(4) = 1.0\n"
        "  result = maxval(a, mask=a(1)>a)\n"
        "end program\n")
    expected = (
        "  result = -HUGE(result)\n"
        "  do idx = 1, 4, 1\n"
        "    if (a(1) > a(idx)) then\n"
        "      result = MAX(result, a(idx))\n"
        "    end if\n"
        "  enddo\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Maxval2LoopTrans()
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.walk(IntrinsicCall)[0]
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_allocate(fortran_reader, fortran_writer, tmpdir):
    '''Test that a newly created array is allocated after the original
    array is allocated (if the original array is allocated). Use the
    Maxval2LoopTrans transformations (a subclass of
    ArrayReductionBaseTrans), as it is easier to test.

    '''
    code = (
        "program sum_test\n"
        "  real, allocatable :: a(:,:,:)\n"
        "  real :: result(4,4)\n"
        "  allocate(a(4,4,4))\n"
        "  result = maxval(a)\n"
        "  deallocate(a)\n"
        "end program\n")
    expected = (
        "  ALLOCATE(a(1:4,1:4,1:4))\n"
        "  result = -HUGE(result)\n"
        "  do idx = LBOUND(a, dim=3), UBOUND(a, dim=3), 1\n"
        "    do idx_1 = LBOUND(a, dim=2), UBOUND(a, dim=2), 1\n"
        "      do idx_2 = LBOUND(a, dim=1), UBOUND(a, dim=1), 1\n"
        "        result = MAX(result, a(idx_2,idx_1,idx))\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n"
        "  DEALLOCATE(a)\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Maxval2LoopTrans()
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.walk(IntrinsicCall)[1]
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_references(fortran_reader, fortran_writer, tmpdir):
    '''Test that the expected result is obtained when the arrays in the
    expression and mask have References. These are internally changed
    to ArrayReferences using the Reference2ArrayRangeTrans
    transformation where required.

    '''
    code = (
        "subroutine test(tmask)\n"
        "real :: sshn(10,10), tmask(:,:)\n"
        "real :: zmax(10)\n"
        "real :: ssh_ref\n"
        "zmax(1) = MAXVAL(ABS(sshn + ssh_ref * tmask), mask=tmask==1.0)\n"
        "end subroutine\n")
    expected = (
        "  zmax(1) = -HUGE(zmax(1))\n"
        "  do idx = 1, 10, 1\n"
        "    do idx_1 = 1, 10, 1\n"
        "      if (tmask(idx_1,idx) == 1.0) then\n"
        "        zmax(1) = MAX(zmax(1), ABS(sshn(idx_1,idx) + ssh_ref * "
        "tmask(idx_1,idx)))\n"
        "      end if\n"
        "    enddo\n"
        "  enddo\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Maxval2LoopTrans()
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.children[0].children[0].children[1]
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_nemo_example(fortran_reader, fortran_writer, tmpdir):
    '''Test a typical nemo example produces the expected code.'''
    code = (
        "subroutine test()\n"
        "real :: sshn(10,10), tmask(10,10,10)\n"
        "real :: zmax(10)\n"
        "real :: ssh_ref\n"
        "zmax(1) = MAXVAL(ABS(sshn(:,:) + ssh_ref * tmask(:,:,1)))\n"
        "end subroutine\n")
    expected = (
        "  zmax(1) = -HUGE(zmax(1))\n"
        "  do idx = LBOUND(sshn, dim=2), UBOUND(sshn, dim=2), 1\n"
        "    do idx_1 = LBOUND(sshn, dim=1), UBOUND(sshn, dim=1), 1\n"
        "      zmax(1) = MAX(zmax(1), ABS(sshn(idx_1,idx) + ssh_ref * "
        "tmask(idx_1,idx,1)))\n"
        "    enddo\n"
        "  enddo\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Maxval2LoopTrans()
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.children[0].children[0].children[1]
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_constant_dims(fortran_reader, fortran_writer, tmpdir):
    '''Test that the code works as expected when one of more of the arrays
    contains a dimension with a constant value rather than a range.

    '''
    code = (
        "subroutine test()\n"
        "real :: a(10,10), b(10,10), c(10)\n"
        "real :: x\n"
        "x = maxval(a(:,1)+b(10,:), mask=c(:)==1.0)\n"
        "end subroutine\n")
    expected = (
        "  x = -HUGE(x)\n"
        "  do idx = LBOUND(a, dim=1), UBOUND(a, dim=1), 1\n"
        "    if (c(idx) == 1.0) then\n"
        "      x = MAX(x, a(idx,1) + b(10,idx))\n"
        "    end if\n"
        "  enddo\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Maxval2LoopTrans()
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.children[0].children[0].children[1]
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_expression_1d(fortran_reader, fortran_writer, tmpdir):
    '''Check that the expected code is produced when the arrays in the
    expressions are one dimensional.

    '''
    code = (
        "subroutine test()\n"
        "real :: a(10), b(10)\n"
        "real :: x\n"
        "x = maxval(a(:)+b(:))\n"
        "end subroutine\n")
    expected = (
        "subroutine test()\n"
        "  real, dimension(10) :: a\n"
        "  real, dimension(10) :: b\n"
        "  real :: x\n"
        "  integer :: idx\n\n"
        "  x = -HUGE(x)\n"
        "  do idx = LBOUND(a, dim=1), UBOUND(a, dim=1), 1\n"
        "    x = MAX(x, a(idx) + b(idx))\n"
        "  enddo\n\n"
        "end subroutine test\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Maxval2LoopTrans()
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.children[0].children[0].children[1]
    trans.apply(node)
    result = fortran_writer(psyir)
    assert result == expected
    assert Compile(tmpdir).string_compiles(result)


def test_expression_3d(fortran_reader, fortran_writer, tmpdir):
    '''Check that the expected code is produced when the arrays in the
    expressions are multi-dimensional (3 dimensions in this case).

    '''
    code = (
        "subroutine test()\n"
        "real :: a(10,10,10)\n"
        "real :: x\n"
        "x = maxval(-a(:,:,:)+10.0)\n"
        "end subroutine\n")
    expected = (
        "subroutine test()\n"
        "  real, dimension(10,10,10) :: a\n"
        "  real :: x\n"
        "  integer :: idx\n"
        "  integer :: idx_1\n"
        "  integer :: idx_2\n\n"
        "  x = -HUGE(x)\n"
        "  do idx = LBOUND(a, dim=3), UBOUND(a, dim=3), 1\n"
        "    do idx_1 = LBOUND(a, dim=2), UBOUND(a, dim=2), 1\n"
        "      do idx_2 = LBOUND(a, dim=1), UBOUND(a, dim=1), 1\n"
        "        x = MAX(x, -a(idx_2,idx_1,idx) + 10.0)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n\n"
        "end subroutine test\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Maxval2LoopTrans()
    # FileContainer/Routine/Assignment/IntrinsicCall
    node = psyir.children[0].children[0].children[1]
    trans.apply(node)
    result = fortran_writer(psyir)
    assert result == expected
    assert Compile(tmpdir).string_compiles(result)


def test_multi_intrinsics(fortran_reader, fortran_writer, tmpdir):
    '''Check that the expected code is produced when there is more than
    one of the same intrinsic on the rhs of the assignment.

    '''
    code = (
        "subroutine test()\n"
        "real :: a(10), b(10)\n"
        "real :: x\n"
        "x = maxval(a(:)) + maxval(b(:))\n"
        "end subroutine\n")
    expected = (
        "  x = -HUGE(x)\n"
        "  do idx = LBOUND(a, dim=1), UBOUND(a, dim=1), 1\n"
        "    x = MAX(x, a(idx))\n"
        "  enddo\n"
        "  x = x + MAXVAL(b(:))\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Maxval2LoopTrans()
    # FileContainer/Routine/Assignment/BinaryOp/IntrinsicCall
    node = psyir.children[0].children[0].children[1].children[0]
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_increment(fortran_reader, fortran_writer, tmpdir):
    '''Check that the expected code is produced when the variable being
    assigned to is an increment e.g. x = x + ...

    '''
    code = (
        "subroutine test()\n"
        "real :: a(10)\n"
        "real :: x\n"
        "x = x + maxval(a)\n"
        "end subroutine\n")
    expected = (
        "  tmp_var = -HUGE(tmp_var)\n"
        "  do idx = 1, 10, 1\n"
        "    tmp_var = MAX(tmp_var, a(idx))\n"
        "  enddo\n"
        "  x = x + tmp_var\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Maxval2LoopTrans()
    node = psyir.walk(IntrinsicCall)[0]
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_increment_with_accessor(fortran_reader, fortran_writer, tmpdir):
    '''Check that the expected code is produced when the variable being
    assigned needs a temporary variable that is not the same type as the
    lhs symbol because some accessor expression is used.

    '''
    code = (
        "subroutine test()\n"
        "real :: a(10)\n"
        "real, dimension(1) :: x\n"
        "x(1) = x(1) + maxval(a)\n"
        "end subroutine\n")
    expected_decl = "real :: tmp_var"
    expected = (
        "  tmp_var = -HUGE(tmp_var)\n"
        "  do idx = 1, 10, 1\n"
        "    tmp_var = MAX(tmp_var, a(idx))\n"
        "  enddo\n"
        "  x(1) = x(1) + tmp_var\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Maxval2LoopTrans()
    node = psyir.walk(IntrinsicCall)[0]
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected_decl in result
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_reduce_to_struct_and_array_accessors(fortran_reader, fortran_writer):
    '''Check that the expected code is produced when the variable being
    assigned to is has array and structure accessors.

    '''
    code = (
        "subroutine test()\n"
        "use other\n"
        "real :: a(10)\n"
        "mystruct%x(3) = maxval(a)\n"
        "end subroutine\n")
    expected = (
        "  mystruct%x(3) = -HUGE(mystruct%x(3))\n"
        "  do idx = 1, 10, 1\n"
        "    mystruct%x(3) = MAX(mystruct%x(3), a(idx))\n"
        "  enddo\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Maxval2LoopTrans()
    node = psyir.walk(IntrinsicCall)[0]
    trans.apply(node)
    result = fortran_writer(psyir)
    assert expected in result


def test_range2loop_fails(fortran_reader, fortran_writer):
    '''Check that the expected error and code is produced when the internal
    range2loop transformation fails to convert the range into a loop construct.

    '''
    code = (
        "subroutine test()\n"
        "use othermod\n"
        "real :: a(10,10)\n"
        "real :: x\n"
        "x = maxval(a(:,:undeclared))\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Maxval2LoopTrans()
    node = psyir.walk(IntrinsicCall)[0]
    code_before = fortran_writer(psyir)
    with pytest.raises(TransformationError) as info:
        trans.apply(node)
    assert ("NemoAllArrayRange2LoopTrans could not convert the expression "
            "'a(:,:undeclared) = a(:,:undeclared)\n' into a loop."
            in str(info.value))
    # Check that the failed transformation does not modify the code
    code_after = fortran_writer(psyir)
    assert code_before == code_after
