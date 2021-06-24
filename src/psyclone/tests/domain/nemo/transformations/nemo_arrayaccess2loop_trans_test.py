# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford, STFC Daresbury Lab

'''Module containing tests for the NemoArrayAccess2LoopTrans
transformation.'''

from __future__ import absolute_import

import os
import pytest

from psyclone.psyGen import Transformation
from psyclone.domain.nemo.transformations import NemoArrayAccess2LoopTrans
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Assignment, ArrayReference, Literal, Node
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, REAL_TYPE, \
    ArrayType
from psyclone.psyir.transformations import TransformationError

# Constants
API = "nemo"
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         os.pardir, "test_files")
TEST_CONFIG = os.path.join(BASE_PATH, "nemo_test.cfg")


def check_transformation(code, expected_result, index=0, statement=0):
    '''Utility function to check that the result of applying the
    NemoArrayAccess2LoopTrans transformation to the code supplied in
    the code argument for the statement number specified in the
    statement argument and the array access index specified in the
    index argument produces the result specified in the
    expected_result argument.

    :param str code: the input code to be transformed.
    :param str expected_result: the code expected after transformation.
    :param int index: the array index on which to apply the \
        transformation.
    :param int statement: the index of the required statement in the top \
        level of the PSyIR tree associated with the input code. \
        Defaults to 0.

    '''
    input_code = "program test\n{0}end program test\n".format(code)
    output_code = "program test\n{0}end program test\n".format(expected_result)
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)
    assignment = psyir.walk(Assignment)[statement]
    array_reference = assignment.lhs
    index_node = array_reference.children[index]

    trans = NemoArrayAccess2LoopTrans()
    trans.apply(index_node)

    writer = FortranWriter()
    result = writer(psyir)
    assert result == output_code


def test_transform():
    '''Check that it is possible to create an instance of
    NemoArrayAccess2LoopTrans and that it is a subclass of
    Transformation.

    '''
    assert NemoArrayAccess2LoopTrans()
    assert isinstance(NemoArrayAccess2LoopTrans(), Transformation)


def test_apply_single_dim_value():
    '''Check that the expected code is produced when there is a 1D array,
    the access to its 1st dimension is an integer value and this 1st
    index is provided to the transformation.

    '''
    code = (
        "  real :: a(10), b(10)\n"
        "  a(1) = b(1)\n")
    expected_result = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  integer :: ji\n\n"
        "  do ji = 1, 1, 1\n"
        "    a(ji) = b(ji)\n"
        "  enddo\n\n")
    check_transformation(code, expected_result)


def test_apply_second_dim_var():
    '''Check that the expected code is produced for a 2D array where the
    access to its 2nd dimension is via a scalar variable (that is not
    a loop iterator) and the 2nd dimension is provided to the
    transformation. Also add some code before the array access to make
    sure there aren't any issues when modifying the tree.

    '''
    code = (
        "  real :: a(10,10), b(10,10)\n"
        "  integer :: n\n"
        "  b(:,:) = 0.0\n"
        "  a(1,n) = b(1,n)\n")
    expected_result = (
        "  real, dimension(10,10) :: a\n  real, dimension(10,10) :: b\n"
        "  integer :: n\n  integer :: jj\n\n"
        "  b(:,:) = 0.0\n"
        "  do jj = n, n, 1\n"
        "    a(1,jj) = b(1,jj)\n"
        "  enddo\n\n")
    check_transformation(code, expected_result, index=1, statement=1)


def test_apply_third_dim_expr():
    '''Check that the expected code is produced for a 3D array where the
    access to its 3rd dimension is via an expression (that does not
    contain a loop iterator) and this dimension is provided to the
    transformation.

    '''
    code = (
        "  real :: a(10,10,10), b(10,10,10)\n"
        "  integer :: n\n"
        "  a(1,n,2*n+1) = b(1,n,2*n+1)\n")
    expected_result = (
        "  real, dimension(10,10,10) :: a\n  real, dimension(10,10,10) :: b\n"
        "  integer :: n\n  integer :: jk\n\n"
        "  do jk = 2 * n + 1, 2 * n + 1, 1\n"
        "    a(1,n,jk) = b(1,n,jk)\n"
        "  enddo\n\n")
    check_transformation(code, expected_result, index=2)


def test_apply_fifth_dim_expr():
    '''Check that the expected code is produced for a 5D array where there
    is no loop index information in the nemo api for the outermost
    dimension so a new loop variable name is required.

    '''
    code = (
        "  real :: a(10,10,10,10,10)\n"
        "  a(1,1,1,1,1) = 0.0\n")
    expected_result = (
        "  real, dimension(10,10,10,10,10) :: a\n"
        "  integer :: idx\n\n"
        "  do idx = 1, 1, 1\n"
        "    a(1,1,1,1,idx) = 0.0\n"
        "  enddo\n\n")
    check_transformation(code, expected_result, index=4)


def test_apply_indirection():
    '''Check that the expected code is produced for a 1D array where the
    access to its 1st dimension index is via a lookup (that does not
    contain a loop iterator) and this dimension is provided to the
    transformation.

    '''
    code = (
        "  real :: a(10), b(10)\n"
        "  integer :: lookup(10)\n"
        "  integer :: n\n"
        "  a(lookup(n)) = b(lookup(n))\n")
    expected_result = (
        "  real, dimension(10) :: a\n"
        "  real, dimension(10) :: b\n"
        "  integer, dimension(10) :: lookup\n"
        "  integer :: n\n"
        "  integer :: ji\n\n"
        "  do ji = lookup(n), lookup(n), 1\n"
        "    a(ji) = b(ji)\n"
        "  enddo\n\n")
    check_transformation(code, expected_result)


def test_apply_loop_order():
    '''Check that the expected code is produced for a 3D array where it is
    accessed with the inner and outer dimensions being loop indices,
    but the middle dimension being a variable and this middle
    dimension is provided to the transformation.

    '''
    code = (
        "  real :: a(10,10,10), b(10,10,10)\n"
        "  integer :: ji, n, jk\n"
        "  do jk =1, 10, 1\n"
        "    do ji = 1, 10, 1\n"
        "      a(ji,n,jk) = b(ji,n,jk)\n"
        "    enddo\n"
        "  enddo\n")
    expected_result = (
        "  real, dimension(10,10,10) :: a\n  real, dimension(10,10,10) :: b\n"
        "  integer :: ji\n  integer :: n\n  integer :: jk\n  integer :: jj\n\n"
        "  do jk = 1, 10, 1\n"
        "    do jj = n, n, 1\n"
        "      do ji = 1, 10, 1\n"
        "        a(ji,jj,jk) = b(ji,jj,jk)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n\n")
    check_transformation(code, expected_result, index=1)


def test_apply_ranges():
    '''Check that the expected code is produced for a 3D array where its
    inner two dimensions are array ranges (array notation), its outer
    dimension is a variable and the outer dimension is provided to the
    transformation.

    '''
    code = (
        "  real :: a(10,10,10), b(10,10,10)\n"
        "  integer :: jpk\n"
        "  a(:,:,jpk) = 0.0e0\n")

    expected_result = (
        "  real, dimension(10,10,10) :: a\n  real, dimension(10,10,10) :: b\n"
        "  integer :: jpk\n  integer :: jk\n\n"
        "  do jk = jpk, jpk, 1\n"
        "    a(:,:,jk) = 0.0e0\n"
        "  enddo\n\n")
    check_transformation(code, expected_result, index=2)


def test_apply_calls_validate():
    '''Check that the apply() method calls the validate method.'''
    trans = NemoArrayAccess2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert("Error in NemoArrayAccess2LoopTrans transformation. The supplied "
           "node argument should be a PSyIR Node, but found 'NoneType'."
           in str(info.value))

# validate() method


def test_validate_arg():
    '''Check that the validate() method raises the expected exception if
   the supplied node is not a PSyIR Node.

    '''
    trans = NemoArrayAccess2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert("Error in NemoArrayAccess2LoopTrans transformation. The "
           "supplied node argument should be a PSyIR Node, but "
           "found 'NoneType'." in str(info.value))


def test_validate_array_ref():
    '''Check that the validate() method raises the expected exception if
   the supplied node is not a child of an array reference.

    '''
    trans = NemoArrayAccess2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(Node())
    assert("The supplied node argument should be within an ArrayReference "
           "node, but found 'NoneType'." in str(info.value))


def test_validate_assignment():
    '''Check that the validate() method raises the expected exception if
    the supplied node is not a child of an array reference that is
    within an assignment.

    '''
    dim_access = Literal("1", INTEGER_TYPE)
    array_symbol = DataSymbol("x", ArrayType(REAL_TYPE, [10]))
    ArrayReference.create(array_symbol, [dim_access])
    trans = NemoArrayAccess2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(dim_access)
    assert(
        "Error in NemoArrayAccess2LoopTrans transformation. The supplied node "
        "argument should be within an ArrayReference node that is within an "
        "Assignment node, but found 'NoneType'." in str(info.value))


def test_validate_lhs_assignment():
    '''Check that the validate() method raises the expected exception if
    the supplied node is a child of an array reference that is
    within an assignment, but is on its right hand side.

    '''
    dim_access = Literal("1", INTEGER_TYPE)
    array_symbol = DataSymbol("x", ArrayType(REAL_TYPE, [10]))
    array_ref = ArrayReference.create(array_symbol, [dim_access])
    lhs = array_ref.copy()
    Assignment.create(lhs, array_ref)
    trans = NemoArrayAccess2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(dim_access)
    assert(
        "Error in NemoArrayAccess2LoopTrans transformation. The supplied "
        "node argument should be within an ArrayReference node that is "
        "within the left-hand-side of an Assignment node, but it is on the "
        "right-hand-side." in str(info.value))


def test_validate_range():
    '''Check that the validate() method raises the expected exception if
    the supplied node is, or contains, a Range node as it should be
    single valued.

    '''
    code = (
        "  real :: a(10)\n"
        "  a(:) = 0.0e0\n")
    expected_result = None
    with pytest.raises(TransformationError) as info:
        check_transformation(code, expected_result)
    assert (
        "Error in NemoArrayAccess2LoopTrans transformation. The supplied "
        "node should not be or contain a Range node as it should be single "
        "valued." in str(info.value))


def test_validate_iterator():
    '''Check that the validate() method raises the expected exception if
    the supplied node is, or contains, a loop iterator.

    '''
    input_code = (
        "program test\n"
        "  real :: a(10)\n"
        "  integer :: i\n"
        "  do i=1,10\n"
        "    a(i) = 0.0e0\n"
        "  end do\n"
        "end program test")
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)
    trans = NemoArrayAccess2LoopTrans()
    index_node = psyir.children[0].children[0].loop_body[0].lhs.children[0]
    with pytest.raises(TransformationError) as info:
        trans.apply(index_node)
    assert(
        "The supplied node should not be or contain a loop iterator, it "
        "should be single valued." in str(info.value))


def test_validate_multi_iterator():
    '''Check that the validate() method raises the expected exception if
    an index before the supplied one has more than one loop iterator.

    '''
    input_code = (
        "program test\n"
        "  real :: a(10,10)\n"
        "  integer :: i,j\n"
        "  do j=1,10\n"
        "    do i=1,10\n"
        "      a(i+j,10) = 0.0e0\n"
        "    end do\n"
        "  end do\n"
        "end program test")
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)
    trans = NemoArrayAccess2LoopTrans()
    outer_loop = psyir.children[0].children[0]
    assignment = outer_loop.loop_body[0].loop_body[0]
    index_node = assignment.lhs.children[1]
    with pytest.raises(TransformationError) as info:
        trans.apply(index_node)
    assert(
        "Only a single iterator per dimension is supported by this "
        "transformation, but found '['i', 'j']'." in str(info.value))


def test_validate_same_iterator():
    '''Check that the validate() method raises the expected exception if
    the same iterator is used in more than one loop index.

    '''
    input_code = (
        "program test\n"
        "  real :: a(10,10,10)\n"
        "  integer :: i\n"
        "  do i=1,5\n"
        "    a(2*i,i+1,10) = 0.0e0\n"
        "  end do\n"
        "end program test")
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)
    trans = NemoArrayAccess2LoopTrans()
    index_node = psyir.children[0].children[0].loop_body[0].lhs.children[2]
    with pytest.raises(TransformationError) as info:
        trans.apply(index_node)
    assert(
        "The same iterator is used in more than one loop index '['i', 'i']' "
        "which is not supported by this transformation." in str(info.value))


def test_validate_index_order_error():
    '''Check that the validate() method raises the expected exception if
    the iterators are not used in index order i.e. x(i,j,k) is correct
    if i is the iterators for the inner loop, j is iterator for the
    middle loop and k is the iterator for the outer loop.

    '''
    input_code = (
        "program test\n"
        "  real :: a(10,10,10)\n"
        "  integer :: i,j\n"
        "  do j=1,10\n"
        "    do i=1,10\n"
        "      a(j,i,10) = 0.0e0\n"
        "    end do\n"
        "  end do\n"
        "end program test")
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)
    trans = NemoArrayAccess2LoopTrans()
    outer_loop = psyir.children[0].children[0]
    assignment = outer_loop.loop_body[0].loop_body[0]
    index_node = assignment.lhs.children[2]
    with pytest.raises(TransformationError) as info:
        trans.apply(index_node)
    assert(
        "In array 'a' expected iterator 'i' at index '0' but found 'j'."
        in str(info.value))


def test_validate_same_index_error():
    '''Check that the validate() method raises the expected exception if
    the indices of the lhs and rhs arrays do not match, but is OK if they do.

    '''
    # Same values
    input_code = (
        "program test\n"
        "  real :: a(10), b(10)\n"
        "  integer :: n\n"
        "  a(n+1) = b(1+n)\n"
        "end program test")
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)
    trans = NemoArrayAccess2LoopTrans()
    index_node = psyir.children[0].children[0].lhs.children[0]
    trans.apply(index_node)

    # different values
    input_code = (
        "program test\n"
        "  real :: a(10), b(10)\n"
        "  integer :: n\n"
        "  a(n+1) = b(n)\n"
        "end program test")
    psyir = reader.psyir_from_source(input_code)
    trans = NemoArrayAccess2LoopTrans()
    index_node = psyir.children[0].children[0].lhs.children[0]
    with pytest.raises(TransformationError) as info:
        trans.apply(index_node)
    assert(
        "Expected index '0' for rhs array 'b' to be the same as the lhs "
        "array 'a', but they differ." in str(info.value))


def test_validate_indirection():
    '''Check that validation works with valid indirection.'''

    code = (
        "program tmp\n"
        "  real :: a(10), b(10)\n"
        "  integer :: lookup(10)\n"
        "  integer :: n\n"
        "  a(lookup(n)) = b(lookup(n))\n"
        "end program tmp\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    trans = NemoArrayAccess2LoopTrans()
    index_node = psyir.children[0].children[0].lhs.children[0]
    trans.validate(index_node)

# str() and name() methods


def test_str():
    '''Test that the str of an instance of the NemoArrayAccess2LoopTrans class
    returns the expected value.

    '''
    assert (str(NemoArrayAccess2LoopTrans()) == "Convert the PSyIR assignment "
            "for a specified ArrayReference access into a PSyIR NemoLoop.")


def test_name():
    '''Check that the name property of the ArrayAccess2LoopTrans class
    returns the expected value.

    '''
    assert NemoArrayAccess2LoopTrans().name == "NemoArrayAccess2LoopTrans"
