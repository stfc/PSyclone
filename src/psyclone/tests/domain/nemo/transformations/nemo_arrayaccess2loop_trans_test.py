# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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

import os
import pytest

from psyclone.domain.nemo.transformations import NemoArrayAccess2LoopTrans, \
    CreateNemoPSyTrans
from psyclone.psyGen import Transformation, InlinedKern
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Assignment, ArrayReference, Literal, Node
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, REAL_TYPE, \
    ArrayType
from psyclone.psyir.transformations import TransformationError
from psyclone.tests.utilities import Compile

# Constants
API = "nemo"
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         os.pardir, "test_files")
TEST_CONFIG = os.path.join(BASE_PATH, "nemo_test.cfg")


def check_transformation(tmpdir, code, expected_result, index=0, statement=0):
    '''Utility function to check that the result of applying the
    NemoArrayAccess2LoopTrans transformation to the code supplied in
    the "code" argument (which is assumed to be the content of a valid
    program) for the statement number specified in the "statement"
    argument and the array access index specified in the "index"
    argument produces the result specified in the "expected_result"
    argument. Also check that the resultant code compiles.

    :param tmpdir: path to a test-specific temporary directory in \
        which to test compilation.
    :type tmpdir: :py:class:`py._path.local.LocalPath`
    :param str code: the input code to be transformed.
    :param str expected_result: the code expected after transformation.
    :param int index: the array index on which to apply the \
        transformation.
    :param int statement: the index of the required assignment in the top \
        level of the PSyIR tree associated with the input code. \
        Defaults to 0.

    '''
    input_code = f"program test\n{code}end program test\n"
    output_code = f"program test\n{expected_result}end program test\n"
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)
    assignment = psyir.walk(Assignment)[statement]
    array_reference = assignment.lhs
    index_node = array_reference.indices[index]
    trans_write_check(psyir, index_node, output_code, tmpdir)


def trans_write_check(psyir, index_node, expected_result, tmpdir,
                      compiles=True):
    '''Utility function to check that the result of applying the
    NemoArrayAccess2LoopTrans transformation to the node supplied in
    the "index_node" argument within the psyir tree supplied in the
    "psyir" argument produces the expected Fortran code provided in
    the "expected_result" argument when output via a Fortran
    writer. It also checks that the output code compiles if the
    compiles argument is set to True.

    :param psyir: the full PSyIR tree.
    :type psyir: :py:class:`psyclone.psyir.nodes.Node`
    :param index_node: the PSyIR node that the transformation will be \
        applied to.
    :type index_node: :py:class:`psyclone.psyir.nodes.Node`
    :param str expected_result: the code that is expected to be \
        produced after the PSyIR is transformed and converted into \
        Fortran.
    :param tmpdir: path to a test-specific temporary directory in \
        which to test compilation.
    :type tmpdir: :py:class:`py._path.local.LocalPath`
    :param bool compiles: whether to compile the resultant code. \
        Defaults to True.

    '''
    trans = NemoArrayAccess2LoopTrans()
    trans.apply(index_node)
    writer = FortranWriter()
    result = writer(psyir)
    assert expected_result in result
    if compiles:
        assert Compile(tmpdir).string_compiles(result)


def test_transform():
    '''Check that it is possible to create an instance of
    NemoArrayAccess2LoopTrans and that it is a subclass of
    Transformation.

    '''
    assert NemoArrayAccess2LoopTrans()
    assert isinstance(NemoArrayAccess2LoopTrans(), Transformation)

# apply() method


def test_apply_single_dim_value(tmpdir):
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
    check_transformation(tmpdir, code, expected_result)


def test_apply_second_dim_var(tmpdir):
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
    check_transformation(tmpdir, code, expected_result, index=1, statement=1)


def test_apply_third_dim_expr(tmpdir):
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
    check_transformation(tmpdir, code, expected_result, index=2)


def test_apply_fifth_dim_expr(tmpdir):
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
    check_transformation(tmpdir, code, expected_result, index=4)


def test_apply_indirection(tmpdir):
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
    check_transformation(tmpdir, code, expected_result)


def test_apply_loop_order(tmpdir):
    '''Check that the expected code is produced for a 3D array where it is
    accessed with the inner and outer dimensions being loop indices,
    but the middle dimension being a variable and this middle
    dimension is provided to the transformation. The loop is simply
    placed innermost so a subsequent transformation will be needed to
    re-order to ji, jj, jk form if required.

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
        "    do ji = 1, 10, 1\n"
        "      do jj = n, n, 1\n"
        "        a(ji,jj,jk) = b(ji,jj,jk)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n\n")
    check_transformation(tmpdir, code, expected_result, index=1)


def test_apply_ranges(tmpdir):
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
    check_transformation(tmpdir, code, expected_result, index=2)


def test_apply_calls_validate():
    '''Check that the apply() method calls the validate method.'''
    trans = NemoArrayAccess2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert ("Error in NemoArrayAccess2LoopTrans transformation. The supplied "
            "node argument should be a PSyIR Node, but found 'NoneType'."
            in str(info.value))


def test_apply_multi_iterator(tmpdir):
    '''Check that the apply method works as expected if a different loop
    index has more than one loop iterator and does not care what name
    is used as long as it is not the same as the iterator name for
    this index.

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
    expected_code = (
        "program test\n"
        "  real, dimension(10,10) :: a\n"
        "  integer :: i\n  integer :: j\n  integer :: jj\n\n"
        "  do j = 1, 10, 1\n"
        "    do i = 1, 10, 1\n"
        "      do jj = 10, 10, 1\n"
        "        a(i + j,jj) = 0.0e0\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n\n"
        "end program test\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)
    outer_loop = psyir.children[0].children[0]
    assignment = outer_loop.loop_body[0].loop_body[0]
    index_node = assignment.lhs.children[1]
    trans_write_check(psyir, index_node, expected_code, tmpdir)


def test_apply_same_iterator(tmpdir):
    '''Check that the apply method works as expected if different loop
    indices use the same iterator in more than one loop index and does
    not care what name is used as long as it is not the same as the
    iterator name for this index.

    '''
    input_code = (
        "program test\n"
        "  real :: a(10,10,10)\n"
        "  integer :: i\n"
        "  do i=1,5\n"
        "    a(2*i,i+1,10) = 0.0e0\n"
        "  end do\n"
        "end program test")
    expected_code = (
        "program test\n"
        "  real, dimension(10,10,10) :: a\n"
        "  integer :: i\n  integer :: jk\n\n"
        "  do i = 1, 5, 1\n"
        "    do jk = 10, 10, 1\n"
        "      a(2 * i,i + 1,jk) = 0.0e0\n"
        "    enddo\n"
        "  enddo\n\n"
        "end program test\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)
    index_node = psyir.children[0].children[0].loop_body[0].lhs.children[2]
    trans_write_check(psyir, index_node, expected_code, tmpdir)


def test_apply_index_order(tmpdir):
    '''Check that the apply method works as expected if loop indices are
    not used in the same order as array dimensions and does not care
    what name is used as long as it is not the same as the iterator
    name for this index.

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
    expected_code = (
        "program test\n"
        "  real, dimension(10,10,10) :: a\n"
        "  integer :: i\n  integer :: j\n  integer :: jk\n\n"
        "  do j = 1, 10, 1\n"
        "    do i = 1, 10, 1\n"
        "      do jk = 10, 10, 1\n"
        "        a(j,i,jk) = 0.0e0\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n\n"
        "end program test\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)
    outer_loop = psyir.children[0].children[0]
    assignment = outer_loop.loop_body[0].loop_body[0]
    index_node = assignment.lhs.children[2]
    trans_write_check(psyir, index_node, expected_code, tmpdir)

# validate() method


def test_validate_arg():
    '''Check that the validate() method raises the expected exception if
   the supplied node is not a PSyIR Node.

    '''
    trans = NemoArrayAccess2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert ("Error in NemoArrayAccess2LoopTrans transformation. The "
            "supplied node argument should be a PSyIR Node, but "
            "found 'NoneType'." in str(info.value))


def test_validate_array_ref():
    '''Check that the validate() method raises the expected exception if
   the supplied node is not a child of an array reference.

    '''
    trans = NemoArrayAccess2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(Node())
    assert ("The supplied node argument should be within an ArrayReference "
            "node, but found 'NoneType'." in str(info.value))


def test_validate_structure_error():
    '''Check that an array within a structure is not supported and causes
    the transformation to raise the expected exception.

    '''
    # It is not possible to use the check_transformation() utility
    # here as the path to the variable index differs due to the
    # structure in the assignment.
    code = (
        "program test\n"
        "  use my_struct, only : x\n"
        "  x%a(1) = x%b(1)\n"
        "end program test\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    assignment = psyir.walk(Assignment)[0]
    array_reference = assignment.lhs
    index_node = array_reference.children[0].indices[0]
    trans = NemoArrayAccess2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(index_node)
    assert ("Error in NemoArrayAccess2LoopTrans transformation. The supplied "
            "node argument should be within an ArrayReference node, but "
            "found 'ArrayMember'" in str(info.value))


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
    assert (
        "Error in NemoArrayAccess2LoopTrans transformation. The supplied node "
        "argument should be within an ArrayReference node that is within an "
        "Assignment node, but found 'NoneType' instead of an Assignment."
        in str(info.value))


def test_validate_lhs_assignment():
    '''Check that the validate() method raises the expected exception if
    the supplied node is a child of an array reference that is
    within an assignment, but is on its right hand side.

    '''
    dim_access = Literal("1", INTEGER_TYPE)
    lhs_array_symbol = DataSymbol("x", ArrayType(REAL_TYPE, [10]))
    rhs_array_symbol = DataSymbol("y", ArrayType(REAL_TYPE, [10]))
    lhs = ArrayReference.create(lhs_array_symbol, [dim_access.copy()])
    rhs = ArrayReference.create(rhs_array_symbol, [dim_access])
    Assignment.create(lhs, rhs)
    trans = NemoArrayAccess2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(dim_access)
    assert (
        "Error in NemoArrayAccess2LoopTrans transformation. The supplied "
        "node argument should be within an ArrayReference node that is "
        "within the left-hand-side of an Assignment node, but 'y(1)' is on "
        "the right-hand-side of 'x(1) = y(1)\n'." in str(info.value))


def test_validate_range():
    '''Check that the validate() method raises the expected exception if
    the supplied node is, or contains, a Range node as it should be
    single valued.

    '''
    code = (
        "  real :: a(10)\n"
        "  a(:) = 0.0e0\n")
    with pytest.raises(TransformationError) as info:
        check_transformation(None, code, None)
    assert (
        "Error in NemoArrayAccess2LoopTrans transformation. The supplied "
        "node should not be or contain a Range node (array notation) as it "
        "should be single valued, but found ':'." in str(info.value))


def test_validate_iterator_name():
    '''Check that the validate() method raises the expected exception if
    the iterator name that should be used for this index is already
    being used as a loop iterator.

    '''
    input_code = (
        "  real :: a(10,10)\n"
        "  integer :: jj\n"
        "  do jj=1,10\n"
        "    a(jj,10) = 0.0e0\n"
        "  end do\n")
    with pytest.raises(TransformationError) as info:
        check_transformation(None, input_code, None, index=1)
    assert (
        "The NEMO API expects index 1 to use the 'jj' iterator variable, "
        "but it is already being used in another index 'a(jj,10)'."
        in str(info.value))


def test_validate_iterator():
    '''Check that the validate() method raises the expected exception if
    the supplied node is, or contains, a loop iterator.

    '''
    input_code = (
        "  real :: a(20)\n"
        "  integer :: i, n\n"
        "  do i=1,10\n"
        "    a(i+n) = 0.0e0\n"
        "  end do\n")
    with pytest.raises(TransformationError) as info:
        check_transformation(None, input_code, None)
    assert (
        "The supplied node should not be or contain a loop iterator, it "
        "should be single valued." in str(info.value))


def test_validate_same_index_error(tmpdir):
    '''Check that the validate() method raises the expected exception if
    the indices of the lhs and rhs arrays do not match, but is OK if they do.

    '''
    # Same values
    input_code = (
        "  real :: a(10), b(10)\n"
        "  integer :: n\n"
        "  a(n+1) = b(1+n)\n")
    expected_result = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  integer :: n\n  integer :: ji\n\n"
        "  do ji = n + 1, n + 1, 1\n"
        "    a(ji) = b(ji)\n"
        "  enddo\n\n")
    check_transformation(tmpdir, input_code, expected_result)

    # different values
    input_code = (
        "  real :: a(10), b(10)\n"
        "  integer :: n\n"
        "  a(n+1) = b(n)\n")
    with pytest.raises(TransformationError) as info:
        check_transformation(None, input_code, None)
    assert (
        "Expected index '0' for rhs array 'b' to be the same as that for "
        "the lhs array 'a', but they differ in 'a(n + 1) = b(n)\n'."
        in str(info.value))


def test_validate_indirection(tmpdir):
    '''Check that validation works with valid indirection and raises an
    exception with invalid indirection.

    '''
    code = (
        "  real :: a(10), b(10)\n"
        "  integer :: lookup(10)\n"
        "  integer :: n\n"
        "  a(lookup(n)) = b(lookup(n))\n")
    expected_result = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  integer, dimension(10) :: lookup\n  integer :: n\n"
        "  integer :: ji\n\n"
        "  do ji = lookup(n), lookup(n), 1\n"
        "    a(ji) = b(ji)\n"
        "  enddo\n\n")
    check_transformation(tmpdir, code, expected_result)

    # different values
    input_code = (
        "  real :: a(10), b(10)\n"
        "  integer :: lookup(10)\n"
        "  integer :: n\n"
        "  a(lookup(n)) = b(lookup(1))\n")
    with pytest.raises(TransformationError) as info:
        check_transformation(None, input_code, None)
    assert (
        "Expected index '0' for rhs array 'b' to be the same as that for the "
        "lhs array 'a', but they differ in 'a(lookup(n)) = b(lookup(1))\n'."
        in str(info.value))

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
