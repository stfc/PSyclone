! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program explicit_do
  implicit none
  integer :: ji, jj, jk
  integer, parameter :: jpi=42, jpj=42, jpk=42
  real :: r
  real, dimension(jpi,jpj,jpk) :: umask

  ! Test code with explicit NEMO-style do loop
  ! This makes sure that the assignment to jpk
  ! becomes shared when all of the body is included
  ! in one OMP parallel section
  jpk = 100
  DO jk = 1, jpk
     DO jj = 1, jpj
        DO ji = 1, jpi
           umask(ji,jj,jk) = ji*jj*jk/r
        END DO
     END DO
  END DO
  DO jk = 1, jpk
     DO jj = 1, jpj
        DO ji = 1, jpi
           umask(ji,jj,jk) = ji*jj*jk/r
        END DO
     END DO
  END DO

end program explicit_do
