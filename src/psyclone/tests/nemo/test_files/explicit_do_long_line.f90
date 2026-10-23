! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program explicit_do_long_line
  implicit none
  integer :: ji, jj, jk
  integer, parameter :: jpi=2, jpj=4, jpk=6
  real :: r
  real, dimension(jpi,jpj,jpk) :: umask

  ! Test code with explicit NEMO-style do loop containing long line
  DO jk = 1, jpk
     DO jj = 1, jpj
        DO ji = 1, jpi
           umask(ji,jj,jk) = ji*jj*jk/r ! This is a comment that takes this line beyond the standard limit of one hundred and thirty two characters.
        END DO
     END DO
  END DO

  ! A line that really is too long, even without a comment
  umask(1:jpi,1:jpj,1:jpk) = umask(jpi-1, jpj-1, jpk-1) + umask(jpi, jpj, jpk) + umask(jpi, jpj, jpk) + umask(jpi, jpj, jpk) + jpi + jpj + jpk + jpi + jpj + jpk

end program explicit_do_long_line
