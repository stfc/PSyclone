# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026, University of Cambridge, UK
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module containing tests for the reduction inference tool.'''

from psyclone.core import Signature, AccessSequence
from psyclone.psyir.nodes import (
    Loop, BinaryOperation, IntrinsicCall)
from psyclone.psyir.tools import ReductionInferenceTool


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
