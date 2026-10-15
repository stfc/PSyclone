! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module count_neighbours_mod
  implicit none
  private

  public :: count_neighbours

  contains
  subroutine count_neighbours(neighbours, current)
    real(kind=8), allocatable, dimension(:,:), intent(inout) :: neighbours
    real(kind=8), allocatable, dimension(:,:), intent(in) :: current
    integer :: xstart
    integer :: xstop
    integer :: ystart
    integer :: ystop
    integer :: i
    integer :: j

    ! Note that the fields have an outer halo, so we only
    ! work on the inner region (ignoring the first and last
    ! row and column).
    xstart = LBOUND(current, dim=1) + 1
    xstop = UBOUND(current, dim=1) - 1
    ystart = LBOUND(current, dim=2) + 1
    ystop = UBOUND(current, dim=2) - 1
    do j = ystart, ystop, 1
      do i = xstart, xstop, 1
        neighbours(i,j) = current(i - 1,j - 1) + current(i,j - 1) + current(i + 1,j - 1) + current(i - 1,j) + current(i + 1,j) + &
&current(i - 1,j + 1) + current(i,j + 1) + current(i + 1,j + 1)
      enddo
    enddo

  end subroutine count_neighbours

end module count_neighbours_mod
