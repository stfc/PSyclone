# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# Authors A. B. G. Chalk STFC Daresbury Lab

'''This module contains the DataNodeExtractTrans class.'''

import pytest

from psyclone.psyir.nodes import Assignment
from psyclone.psyir.transformations import (
    DataNodeExtractTrans, TransformationError
)


def test_datanodeextracttrans_validate(fortran_reader):
    """Tests the validate function of the DataNodeExtractTrans."""
    dtrans = DataNodeExtractTrans()
    code = """subroutine test(a, b, c)
        integer, dimension(:,:), intent(inout) :: a, b, c
        c = b + a
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as err:
        dtrans.validate(assign.rhs)
    assert ("Input node's datatype is an array of unknown size, so the "
            "DataNodeExtractTrans cannot be applied. Input node was "
            "'b + a'" in str(err.value))

    code = """subroutine test
        use some_mod
        c = b + a
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as err:
        dtrans.validate(assign.rhs)
    assert ("Input node's datatype cannot be computed, so the "
            "DataNodeExtractTrans cannot be applied. Input node "
           "was 'b + a'" in str(err.value))


def test_datanodeextractrans_apply(fortran_reader, fortran_writer):
    """Tests the apply function of the DataNodeExtractTrans."""
    dtrans = DataNodeExtractTrans()
    code = """subroutine test()
        integer, dimension(10,100) :: a
        integer, dimension(100,10) :: b
        integer, dimension(10,10) :: c, d
        d = c + MATMUL(a, b)
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    dtrans.apply(assign.rhs.operands[1])
    out = fortran_writer(psyir)
    assert ("integer, dimension(SIZE(a, dim=1),SIZE(b, dim=2)) :: tmp"
            in out)
    assert "tmp = MATMUL(a, b)" in out
    assert "d = c + tmp" in out

    code = """subroutine test()
        real :: a
        integer :: b

        b = INT(a)
        end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    dtrans.apply(assign.rhs, storage_name="temporary")
    out = fortran_writer(psyir)
    assert "integer :: temporary" in out
    assert "temporary = INT(a)" in out
    assert "b = temporary" in out
