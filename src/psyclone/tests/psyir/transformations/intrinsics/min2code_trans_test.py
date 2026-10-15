# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing tests for the Min2CodeTrans transformation.'''


from psyclone.psyir.nodes import IntrinsicCall, BinaryOperation
from psyclone.psyir.transformations import Min2CodeTrans
from psyclone.psyir.transformations.intrinsics.minormax2code_trans import \
    MinOrMax2CodeTrans


def test_initialise():
    '''Check that the class Min2CodeTrans behaves as expected when an
    instance of the class is created.

    '''
    assert issubclass(Min2CodeTrans, MinOrMax2CodeTrans)
    trans = Min2CodeTrans()
    assert trans._intrinsic == IntrinsicCall.Intrinsic.MIN
    assert trans._compare_operator == BinaryOperation.Operator.LT


def test_apply(fortran_reader, fortran_writer):
    '''Test that applying the Min2CodeTrans behaves as expected.
    '''
    code = """subroutine test
        integer :: i, j, k, l
        k = MIN(i,j, l)
    end subroutine"""

    psyir = fortran_reader.psyir_from_source(code)
    trans = Min2CodeTrans()
    trans.apply(psyir.children[0].children[0].rhs)
    correct = """subroutine test()
  integer :: i
  integer :: j
  integer :: k
  integer :: l
  integer :: res_min
  integer :: tmp_min

  res_min = i
  tmp_min = j
  if (tmp_min < res_min) then
    res_min = tmp_min
  end if
  tmp_min = l
  if (tmp_min < res_min) then
    res_min = tmp_min
  end if
  k = res_min

end subroutine test"""
    assert correct in fortran_writer(psyir)
