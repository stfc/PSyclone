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
# Authors A. B. G. Chalk, STFC Daresbury Lab

'''This module contains the tests for the MaximalOMPParallelRegionTrans.'''

from psyclone.psyir.nodes import (
    Loop,
    OMPBarrierDirective,
    OMPParallelDirective,
)
from psyclone.psyir.transformations import (
    MaximalOMPParallelRegionTrans,
    OMPLoopTrans
)


def test_maximal_ompparallel_region_trans_apply(fortran_reader):
    ''' Test the apply method of the ompparallel region transformation.'''
    code = """subroutine x
    integer :: i
    i = 1
    end subroutine x"""
    psyir = fortran_reader.psyir_from_source(code)
    MaximalOMPParallelRegionTrans().apply(psyir.children[0].children[:])
    assert len(psyir.walk(OMPParallelDirective)) == 0

    # Test that we only get a single parallel region when we have Ifblocks
    # and loops around some of the parallel do region.
    code = """subroutine x(arr)
    integer :: i, j, k
    integer, dimension(:,:,:) :: arr

    !Adding omp do here.
    do i = 1, 100
        do j = 1, 100
            do k = 1, 100
               arr(k,j,i) = 1
            end do
        end do
    end do

    if(.true.) then
      !Adding omp do here.
        do i = 1, 100
            do j = 1, 100
                do k = 1, 100
                   arr(k,j,i) = 2
                end do
            end do
        end do
    else
      !Adding omp do here.
        do i = 1, 100
            do j = 1, 100
                do k = 1, 100
                   arr(k,j,i) = 2
                end do
            end do
        end do
    end if

    do i = 1, 100
      !Adding omp do here.
      do j = 1, 100
          do k = 1, 100
             arr(k,j,i) = 2
          end do
      end do

    end do

    end subroutine x"""

    psyir = fortran_reader.psyir_from_source(code)
    ltrans = OMPLoopTrans()
    loops = psyir.walk(Loop)
    # Add omp do directives to the comments loops in the code fragment.
    ltrans.apply(loops[0], collapse=True)
    ltrans.apply(loops[3], collapse=True)
    ltrans.apply(loops[6], collapse=True)
    ltrans.apply(loops[10], collapse=True)
    # Apply the maximal parallel region trans to the code.
    MaximalOMPParallelRegionTrans().apply(psyir.children[0].children[:])
    # The routine should now have one child and it should be the only
    # OMPParallelDirective
    assert len(psyir.walk(OMPParallelDirective)) == 1
    assert len(psyir.children[0].children) == 1
    assert isinstance(psyir.children[0].children[0], OMPParallelDirective)

    # Check that we don't get a parallel directive around only barriers.
    code = """subroutine x
    integer :: i, j, k
    integer, dimension(:,:,:) :: arr

    !Adding omp do here.
    do i = 1, 100
        do j = 1, 100
            do k = 1, 100
               arr(k,j,i) = 1
            end do
        end do
    end do

    ! parallel region won't go past the assignment here.
    arr = 2

    ! Add some barriers here.
    end subroutine x"""
    psyir = fortran_reader.psyir_from_source(code)
    loops = psyir.walk(Loop)
    ltrans.apply(loops[0], collapse=True)
    # Add two barriers at the end
    psyir.children[0].addchild(OMPBarrierDirective())
    psyir.children[0].addchild(OMPBarrierDirective())
    # Apply the maximal parallel region trans to the code.
    MaximalOMPParallelRegionTrans().apply(psyir.children[0].children[:])
    # The routine should now have four children and the first should be the
    # only OMPParallelDirective, and the last two still the barriers.
    assert len(psyir.walk(OMPParallelDirective)) == 1
    assert len(psyir.children[0].children) == 4
    assert isinstance(psyir.children[0].children[0], OMPParallelDirective)
    assert isinstance(psyir.children[0].children[2], OMPBarrierDirective)
    assert isinstance(psyir.children[0].children[3], OMPBarrierDirective)
