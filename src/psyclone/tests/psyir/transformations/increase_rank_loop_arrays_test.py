# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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
# Author: S. Siso, STFC Daresbury Lab

''' Module containing tests for the IncreaseRankLoopArraysTrans class. '''

import pytest
from psyclone.psyir.nodes import Assignment, Loop
from psyclone.psyir.transformations import IncreaseRankLoopArraysTrans
from psyclone.transformations import TransformationError


def test_trans_name_and_str():
    ''' Check that the name and str method works as expected. '''
    trans = IncreaseRankLoopArraysTrans()
    assert trans.name == "IncreaseRankLoopArraysTrans"
    assert str(trans) == (
        "Increases the Rank of the supplied arrays by the iteration space of "
        "the given loop, and update all its references")


def test_irla_validate(fortran_reader):
    ''' Check that the validate method works as expected. '''
    trans = IncreaseRankLoopArraysTrans()

    psyir = fortran_reader.psyir_from_source("""
        program test
            use other
            goto 3
            do i = 1, 10
                a = 3
            end do
        end program
    """)

    with pytest.raises(TransformationError) as err:
        trans.apply(psyir.walk(Assignment)[0])
    assert ("The target of the IncreaseRankLoopArraysTrans transformation "
            "should be a Loop, but found 'Assignment'." in str(err.value))

    # with pytest.raises(TransformationError) as err:
    #     trans.apply(psyir.walk(Loop)[0])
    # assert ("The supplied loop should be inside a routine, and the whole "
    #         "routine should have no CodeBlocks." in str(err.value))

    # with pytest.raises(TransformationError) as err:
    #     trans.apply(psyir.walk(Loop)[0].detach())
    # assert ("The supplied loop should be inside a routine, and the whole "
    #         "routine should have no CodeBlocks." in str(err.value))


def test_irla_apply(fortran_reader, fortran_writer):
    ''' Check that the array rank is increased by the bounds of the loop. '''
    trans = IncreaseRankLoopArraysTrans()

    psyir = fortran_reader.psyir_from_source("""
     program test
         integer :: N=10, M=10
         integer :: i, j
         real, dimension(N) :: ztmp
         do i = -5, M+3
             do j = 1, N
                 ztmp(j) = 1
             end do
             do j = 1, N
                 ztmp(j) = ztmp(j) + 1
             end do
         end do
     end program
    """)

    trans.apply(psyir.walk(Loop)[0], options={'arrays': ['ztmp']})
    code = fortran_writer(psyir)

    # The declaration and references have been updated
    assert "real, dimension(n,-5:m + 3) :: ztmp" in code
    assert "ztmp(j,i) = 1" in code
    assert "ztmp(j,i) = ztmp(j,i) + 1" in code
