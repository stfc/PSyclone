! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Test code with example include statement
program include_eg
  implicit none
  integer, parameter :: jpi=21, jpj=32, jpk=43
  real, dimension(jpi,jpj,jpk) :: umask

  include 'local_mpi.h'
  
  umask(1,1,1) = 0.0

end program include_eg
