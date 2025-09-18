# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, University of Cambridge, UK
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
# Author M. Naylor, University of Cambridge, UK

''' Module containing tests for the reduction inference tool.'''

import pytest

from psyclone.configuration import Config
from psyclone.core import Signature, AccessSequence
from psyclone.psyir.nodes import (
    Loop, BinaryOperation, IntrinsicCall)
from psyclone.psyir.tools import ReductionInferenceTool


@pytest.fixture(scope="function", autouse=True)
def clear_config_instance():
    '''The tests in this file all assume that no DSL API is used.'''
    Config.get().api = ""


# -----------------------------------------------------------------------------
def test_attempt_reduction(fortran_reader, fortran_writer):
    ''' Test that attempt_reduction() succeeds in a basic case.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sum_arr(arr) result (acc1)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: acc1 = 0
            integer :: acc2 = 0
            integer :: acc3 = 0
            integer :: acc4 = 0

            do i = 1, ubound(arr)
                acc = acc * arr(i)
                acc1 = acc1 + arr(i)
                acc2 = MAX(acc2, arr(i))
                acc3 = acc3 + acc3
                acc4 = acc4 + arr(i)
                acc4 = MAX(acc4, arr(i))
            end do
        end function''')
    loop = psyir.walk(Loop)[0]
    red_infer_tool = ReductionInferenceTool(
                       [BinaryOperation.Operator.ADD,
                        IntrinsicCall.Intrinsic.MAX])
    for (sig, access_seq) in loop.reference_accesses().items():
        clause = red_infer_tool.attempt_reduction(sig, access_seq)
        # Variables acc1 and acc2 both allow reductions
        if str(sig) == "acc1" or str(sig) == "acc2":
            assert clause is not None
        # Variable acc allows a multiply reduction, but multiply was not
        # specified as a valid reduction variable
        if str(sig) == "acc":
            assert clause is None
        # Variable acc3 should not allow a reduction
        if str(sig) == "acc3":
            assert clause is None
        # Variable acc4 involves reductions with different operators,
        # which should not lead to a reduction clause
        if str(sig) == "acc4":
            assert clause is None


# -----------------------------------------------------------------------------
def test_attempt_reduction_no_accesses(fortran_reader, fortran_writer):
    ''' Test that attempt_reduction() fails when given empty access info.
    '''
    sig = Signature("foo")
    empty_access_sequence = AccessSequence(sig)
    red_infer_tool = ReductionInferenceTool([])
    clause = red_infer_tool.attempt_reduction(sig, empty_access_sequence)
    assert clause is None
