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
# Authors: R. W. Ford and N. Nobre, STFC Daresbury Lab

'''Module containing tests for the NemoAllArrayAccess2LoopTrans
transformation.'''

from __future__ import absolute_import

import os
import pytest

from psyclone.domain.nemo.transformations import NemoAllArrayAccess2LoopTrans
from psyclone.psyGen import Transformation
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Assignment
from psyclone.psyir.transformations import TransformationError
from psyclone.tests.utilities import Compile

# Constants
API = "nemo"
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         os.pardir, "test_files")
TEST_CONFIG = os.path.join(BASE_PATH, "nemo_test.cfg")


def check_transformation(tmpdir, code, expected_result, statement=0):
    '''Utility function to check that the result of applying the
    NemoAllArrayAccess2LoopTrans transformation to the code supplied in
    the "code" argument (which is assumed to be the content of a valid
    program) for the statement number specified in the "statement" argument
    produces the result specified in the "expected_result" argument. Also
    check that the resultant code compiles.

    :param tmpdir: path to a test-specific temporary directory in \
        which to test compilation.
    :type tmpdir: :py:class:`py._path.local.LocalPath`
    :param str code: the input code to be transformed.
    :param str expected_result: the code expected after transformation.
    :param int statement: the index of the required assignment in the top \
        level of the PSyIR tree associated with the input code. \
        Defaults to 0.

    '''
    input_code = f"program test\n{code}end program test\n"
    output_code = f"program test\n{expected_result}end program test\n"
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)
    assignment = psyir.walk(Assignment)[statement]

    trans = NemoAllArrayAccess2LoopTrans()
    trans.apply(assignment)

    writer = FortranWriter()
    result = writer(psyir)
    assert result == output_code

    assert Compile(tmpdir).string_compiles(result)


def test_transform():
    '''Check that it is possible to create an instance of
    NemoAllArrayAccess2LoopTrans and that it is a subclass of
    Transformation.

    '''
    assert NemoAllArrayAccess2LoopTrans()
    assert isinstance(NemoAllArrayAccess2LoopTrans(), Transformation)


def test_apply_no_change(tmpdir):
    '''Check that no modifications are made to the code when the
    assignment is not an array or the array indices are not constant.

    '''
    # scalars
    code = (
        "  real :: a(10), b(10)\n"
        "  a = b\n")
    expected_result = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n\n"
        "  a = b\n\n")
    check_transformation(tmpdir, code, expected_result)
    # array with no constant indices
    code = (
        "  real :: a(10,10), b(10,10)\n"
        "  integer :: n, ji, jj\n"
        "  do jj = 1, n\n"
        "    do ji = 1, n\n"
        "      a(ji,jj) = b(ji,jj)\n"
        "    end do\n"
        "  end do\n")
    expected_result = (
        "  real, dimension(10,10) :: a\n  real, dimension(10,10) :: b\n"
        "  integer :: n\n  integer :: ji\n  integer :: jj\n\n"
        "  do jj = 1, n, 1\n"
        "    do ji = 1, n, 1\n"
        "      a(ji,jj) = b(ji,jj)\n"
        "    enddo\n"
        "  enddo\n\n")
    check_transformation(tmpdir, code, expected_result, statement=0)


def test_apply_multi_change(tmpdir):
    '''Check that both dimensions are modified when there is a constant
    index in each.

    '''
    code = (
        "  real :: a(10,10), b(10,10)\n"
        "  integer :: n\n"
        "  a(1,n) = b(1,n)\n")
    expected_result = (
        "  real, dimension(10,10) :: a\n  real, dimension(10,10) :: b\n"
        "  integer :: n\n  integer :: ji\n  integer :: jj\n\n"
        "  do ji = 1, 1, 1\n"
        "    do jj = n, n, 1\n"
        "      a(ji,jj) = b(ji,jj)\n"
        "    enddo\n"
        "  enddo\n\n")
    check_transformation(tmpdir, code, expected_result, statement=0)


def test_apply_mixed(tmpdir):
    '''Check that both dimensions are modified when there is a constant
    index in two dimensions and an existing loop in another. The
    generated code does not have the loops in the order that might be
    expected for most efficient memory access and a separate loop
    re-order transformation would need to be applied if required.

    '''
    code = (
        "  real :: a(10,10,10), b(10,10,10)\n"
        "  integer :: n, jj\n"
        "  do jj = 1, n\n"
        "    a(1,jj,n) = b(1,jj,n)\n"
        "  end do\n")
    expected_result = (
        "  real, dimension(10,10,10) :: a\n  real, dimension(10,10,10) :: b\n"
        "  integer :: n\n  integer :: jj\n  integer :: ji\n  integer :: jk\n\n"
        "  do jj = 1, n, 1\n"
        "    do ji = 1, 1, 1\n"
        "      do jk = n, n, 1\n"
        "        a(ji,jj,jk) = b(ji,jj,jk)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n\n")
    check_transformation(tmpdir, code, expected_result, statement=0)


def test_apply_validate():
    '''Check that the validate() method is called from the apply()
    method.

    '''
    trans = NemoAllArrayAccess2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert ("Error in NemoAllArrayAccess2LoopTrans transformation. The "
            "supplied node argument should be a PSyIR Assignment, but "
            "found 'NoneType'." in str(info.value))

# validate() method


def test_validate_arg():
    '''Check that the validate() method raises the expected exception if
   the supplied node is not a PSyIR Assignment Node.

    '''
    trans = NemoAllArrayAccess2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert ("Error in NemoAllArrayAccess2LoopTrans transformation. The "
            "supplied node argument should be a PSyIR Assignment, but "
            "found 'NoneType'." in str(info.value))

# str() and name() methods


def test_str():
    '''Test that the str of an instance of the
    NemoAllArrayAccess2LoopTrans class returns the expected value.

    '''
    assert (str(NemoAllArrayAccess2LoopTrans()) == "Convert the constant "
            "indices of a PSyIR array assignment into NemoLoops.")


def test_name():
    '''Check that the name property of the NemoAllArrayAccess2LoopTrans
    class returns the expected value.

    '''
    assert (NemoAllArrayAccess2LoopTrans().name ==
            "NemoAllArrayAccess2LoopTrans")
