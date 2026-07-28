# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

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
