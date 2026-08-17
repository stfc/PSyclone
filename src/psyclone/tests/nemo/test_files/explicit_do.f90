! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program explicit_do
  implicit none
  integer :: ji, jj, jk
  integer, parameter :: jpi=2, jpj=4, jpk=6
  real :: r
  real, dimension(jpi,jpj,jpk) :: umask

  ! Test code with explicit NEMO-style do loop
  DO jk = 1, jpk
     DO jj = 1, jpj
        DO ji = 1, jpi
           umask(ji,jj,jk) = ji*jj*jk/r
        END DO
     END DO
  END DO

end program explicit_do
