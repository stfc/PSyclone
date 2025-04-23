# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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
# ----------------------------------------------------------------------------
# Author: A. R. Porter, STFC Daresbury Lab

'''
'''

import pytest

from psyclone.psyir.nodes import IntrinsicCall, Reference, Return
from psyclone.psyir.symbols import Symbol
from psyclone.psyir.transformations import ReplaceExprnWithTmpTrans
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)


def test_replace_exprn_validate(fortran_reader):
    '''
    Test that the validate() method behaves as expected.
    '''
    rexptmptrans = ReplaceExprnWithTmpTrans()
    with pytest.raises(TransformationError) as err:
        rexptmptrans.validate(Return())
    assert ("The target of the ReplaceExprnWithTmpTrans transformation should "
            "be a DataNode but found 'Return'" in str(err.value))
    with pytest.raises(TransformationError) as err:
        rexptmptrans.validate(Reference(Symbol("puzzle")))
    assert ("Cannot apply ReplaceExprnWithTmpTrans to expression 'puzzle' "
            "because the type of the result is unknown (UnresolvedType)"
            in str(err.value))


def test_replace_exprn_apply(fortran_reader, fortran_writer):
    '''
    '''
    rexptmptrans = ReplaceExprnWithTmpTrans()
    code = ("module test_mod\n"
            "contains\n"
            "  subroutine test_sub(n)\n"
            "  integer :: i,j,n\n"
            "  integer :: qp1, qp2\n"
            "  real :: a(n,n,2)\n"
            "  real :: b(n)\n"
            "  real :: dj(n,n)\n"
            "  real :: value = 1.0\n"
            "  value = dot_product( &\n"
            "    matmul( a(:,:,qp1), b ), &\n"
            "    matmul( a(:,:,qp2), b ) ) &\n"
            "    / ( dj(qp1,qp2)**2 )\n"
            "  end subroutine test_sub\n"
            "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    for icall in psyir.walk(IntrinsicCall):
        if icall.intrinsic == IntrinsicCall.Intrinsic.MATMUL:
            rexptmptrans.apply(icall)

    output = fortran_writer(psyir)
    assert "real, allocatable, dimension(:) :: ptmp" in output
    assert "real, allocatable, dimension(:) :: ptmp_1" in output
    assert "ptmp = matmul( a(:,:,qp1), b )" in output
    assert "dot_product( ptmp, ptmp_1 )" in output
