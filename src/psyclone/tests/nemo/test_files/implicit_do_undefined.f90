! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! This code demonstrates an implicit loop (as all three of the umask
! indices are specified with a ':' when setting its values to 0) with
! array bounds of unknown size within the subroutine.

subroutine implicit_do(umask)
  ! Example of an implicit loop where the bounds of the array have not
  ! been specified.
  implicit none
  real(kind=kind(1.0d0)), dimension(:,:,:) :: umask

  umask(:,:,:) = 0.0d0

end subroutine implicit_do
