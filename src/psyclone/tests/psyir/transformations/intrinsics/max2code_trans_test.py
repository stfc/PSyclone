# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing tests for the Max2CodeTrans transformation.'''

from psyclone.psyir.nodes import BinaryOperation, IntrinsicCall
from psyclone.psyir.transformations import Max2CodeTrans
from psyclone.psyir.transformations.intrinsics.minormax2code_trans import \
    MinOrMax2CodeTrans


def test_initialise():
    '''Check that the class Max2CodeTrans behaves as expected when an
    instance of the class is created.

    '''
    assert issubclass(Max2CodeTrans, MinOrMax2CodeTrans)
    trans = Max2CodeTrans()
    assert trans._intrinsic == IntrinsicCall.Intrinsic.MAX
    assert trans._compare_operator == BinaryOperation.Operator.GT


def test_apply(fortran_reader, fortran_writer):
    '''Test that applying the Max2CodeTrans behaves as expected.
    '''
    code = """subroutine test
        integer :: i, j, k, l
        k = MAX(i,j, l)
    end subroutine"""

    psyir = fortran_reader.psyir_from_source(code)
    trans = Max2CodeTrans()
    trans.apply(psyir.children[0].children[0].rhs)
    correct = """subroutine test()
  integer :: i
  integer :: j
  integer :: k
  integer :: l
  integer :: res_max
  integer :: tmp_max

  res_max = i
  tmp_max = j
  if (tmp_max > res_max) then
    res_max = tmp_max
  end if
  tmp_max = l
  if (tmp_max > res_max) then
    res_max = tmp_max
  end if
  k = res_max

end subroutine test"""
    assert correct in fortran_writer(psyir)
