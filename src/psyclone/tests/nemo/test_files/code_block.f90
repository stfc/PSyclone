! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program code_block
  implicit none
  integer :: ji, jj, jk, iloop
  integer, parameter :: jpi=10, jpj=10, jpk=10
  real :: r
  real, allocatable, dimension(:,:,:) :: umask

  ! Test code with explicit NEMO-style do loop as well as some general,
  ! executable statements

  write (*,*) "Hello world"
  allocate(umask(jpi,jpj,jpk))

  umask(1,1,:) = 0.0d0
  umask(1,1,1) = -10.0d0

  do jk = 1, jpk
     do jj = 1, jpj
        do ji = 1, jpi
           umask(ji,jj,jk) = ji*jj*jk/r
        end do
     end do
  end do

  do iloop = 1, jpi
     write (*,*) "This is not a kernel"
  end do
  
  write (*,*) "Goodbye world"
  deallocate(umask)

end program code_block
