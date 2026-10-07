! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program implicit_do
  implicit none
  integer, parameter :: jpi=10, jpj=10, jpk=10
  real(kind=kind(1.0d0)), dimension(jpi,jpj,jpk) :: umask

  ! Test code with implicit NEMO-style do loop
  umask(:,:,:) = 0.0d0

end program implicit_do
