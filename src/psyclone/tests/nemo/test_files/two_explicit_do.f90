! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program explicit_do
  implicit none
  integer :: ji, jj, jk
  integer, parameter :: jpi=3, jpj=6, jpk=9
  real, parameter :: r = 1.0
  real, dimension(jpi,jpj,jpk) :: umask

  ! Test code with two explicit NEMO-style do loops
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
           umask(ji,jj,jk) = 0.0
        END DO
     END DO
  END DO

end program explicit_do
