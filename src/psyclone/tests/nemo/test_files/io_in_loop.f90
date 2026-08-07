! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program io_in_loop
  implicit none
  integer :: ji, jj, jk
  integer, parameter :: jpi=4, jpj=6, jpk=8
  real :: r
  real, dimension(jpi,jpj,jpk) :: umask, vmask

  ! Test code with valid NEMO kernels as well as some that must be
  ! discounted because they contain IO statements
  DO jk = 1, jpk, 1
     DO jj = 1, jpj
        DO ji = 1, jpi
           umask(ji,jj,jk) = ji*jj*jk/r
        END DO
     END DO
  END DO

  DO jk = 1, jpk
     DO jj = 1, jpj
        DO ji = 1, jpi
           umask(ji,jj,jk) = 0.0d0
           write(*,*) umask(ji,jj,jk)
        END DO
     END DO
  END DO

  DO jk = 1, jpk
     DO jj = 1, jpj
        DO ji = 1, jpi
           umask(ji,jj,jk) = -1.0d0
           read(23,*) vmask(ji,jj,jk)
        END DO
     END DO
  END DO

end program io_in_loop
