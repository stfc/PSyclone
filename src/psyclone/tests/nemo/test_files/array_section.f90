! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

subroutine array_section()
  integer :: ji, dummy, n
  real, dimension(:,:) :: a, b, c

  a(:,:) = b(:,:) * c(:,:)

  do ji = 1, n
     a(ji,:) = b(ji,:) * c(ji,:)
  end do

end subroutine array_section
