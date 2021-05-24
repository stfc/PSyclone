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

from psyclone.psyir.nodes import Assignment
from psyclone.psyGen import Transformation
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, REAL_TYPE, \
    ArrayType, DeferredType
from psyclone.psyir.transformations import TransformationError
from psyclone.domain.nemo.transformations.nemo_arrayrange2loop_trans \
    import get_outer_index
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import get_invoke
from psyclone.nemo import NemoKern, NemoLoop
from psyclone.psyir.nodes import Schedule
from psyclone.errors import InternalError
from psyclone.configuration import Config


from psyclone.domain.nemo.transformations import NemoArrayAccess2LoopTrans
from psyclone.psyir.frontend.fortran import FortranReader

# Constants
API = "nemo"
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         os.pardir, "test_files")
TEST_CONFIG = os.path.join(BASE_PATH, "nemo_test.cfg")


def test_transform():
    '''Check that it is possible to create an instance of
    NemoArrayAccess2LoopTrans and that it is a Transformation.

    '''
    assert NemoArrayAccess2LoopTrans()
    assert isinstance(NemoArrayAccess2LoopTrans(), Transformation)

def check_transformation(code, expected_result, index):
    ''' xxx '''
    input_code = "program test\n{0}end program test\n".format(code)
    output_code = "program test\n{0}end program test\n".format(expected_result)
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)
    assignment = psyir.walk(Assignment)[0]
    array_reference = assignment.lhs
    index_node = array_reference.children[index]

    trans = NemoArrayAccess2LoopTrans()
    trans.apply(index_node)

    writer = FortranWriter()
    result = writer(psyir)
    print (result)
    assert result == output_code


def test_example1():
    ''' first dimension integer '''
    code = (
        "  real :: a(10), b(10)\n"
        "  a(1) = b(1)\n")
    expected_result = (
        "  real, dimension(10) :: a\n"
        "  real, dimension(10) :: b\n"
        "  integer :: ji\n\n"
        "  do ji = 1, 1, 1\n"
        "    a(ji) = b(ji)\n"
        "  enddo\n\n")
    check_transformation(code, expected_result, 0)


def test_example2():
    ''' second dimension variable '''
    code = (
        "  real :: a(10), b(10)\n"
        "  integer :: n\n"
        "  a(1,n) = b(1,n)\n")
    expected_result = (
        "  real, dimension(10) :: a\n"
        "  real, dimension(10) :: b\n"
        "  integer :: n\n"
        "  integer :: jj\n\n"
        "  do jj = n, n, 1\n"
        "    a(1,jj) = b(1,jj)\n"
        "  enddo\n\n")
    check_transformation(code, expected_result, 1)


def test_example3():
    ''' third dimension expression '''
    code = (
        "  real :: a(10), b(10)\n"
        "  integer :: n\n"
        "  a(1,n,2*n+1) = b(1,n,2*n+1)\n")
    expected_result = (
        "  real, dimension(10) :: a\n"
        "  real, dimension(10) :: b\n"
        "  integer :: n\n"
        "  integer :: jk\n\n"
        "  do jk = 2 * n + 1, 2 * n + 1, 1\n"
        "    a(1,n,jk) = b(1,n,jk)\n"
        "  enddo\n\n")
    check_transformation(code, expected_result, 2)


# pre-existing loops
def test_example4():
    ''' pre-existing loops '''
    code = (
        "  real :: a(10,10,10), b(10,10,10)\n"
        "  integer :: ji, n, jk\n"
        "  do jk =1, 10, 1\n"
        "    do ji = 1, 10, 1\n"
        "      a(ji,n,jk) = b(ji,n,jk)\n"
        "    enddo\n"
        "  enddo\n")
    expected_result = (
        "  real, dimension(10,10,10) :: a\n"
        "  real, dimension(10,10,10) :: b\n"
        "  integer :: ji\n"
        "  integer :: n\n"
        "  integer :: jk\n"
        "  integer :: jj\n\n"
        "  do jk = 1, 10\n"
        "    do jj = n, n\n"
        "      do jj = 1, 10\n"
        "        a(ji,jj,jk) = b(ji,jj,jk)\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n\n")
    check_transformation(code, expected_result, 1)


# validate (constraint - all indices are the same)

# def test_apply_calls_validate():
#    '''Check that the apply() method calls the validate method.'''
#    trans = NemoArrayRange2LoopTrans()
#    with pytest.raises(TransformationError) as info:
#        trans.apply(None)
#    assert("Error in NemoArrayRange2LoopTrans transformation. The supplied "
#           "node argument should be a PSyIR Range, but found 'NoneType'."
#           in str(info.value))


#def test_str():
#    '''Test that the str of an instance of the NemoArrayRange2LoopTrans class
#    returns the expected value.
#
#    '''
#    assert (str(NemoArrayRange2LoopTrans()) == "Convert the PSyIR assignment "
#            "for a specified ArrayReference Range into a PSyIR NemoLoop.")


#def test_name():
#    '''Check that the name property of the ArrayRange2LoopTrans class
#    returns the expected value.
#
#    '''
#    assert NemoArrayRange2LoopTrans().name == "NemoArrayRange2LoopTrans"
