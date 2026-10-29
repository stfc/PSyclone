! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program implicit_do
  use some_mod, only: ptr_sjk
  implicit none
  integer, parameter :: jpi=10, jpj=10, jpk=10, jpn=2
  integer :: ji, jk, jn
  real(kind=kind(1.0d0)), dimension(jpi,jpj,jpk) :: umask
  real(kind=kind(1.0d0)), dimension(jpi,jpj,jpk) :: z3d
  integer, dimension(jpi,jpj,jpk) :: pvtr
  integer, dimension(jpi,jpj) :: btm30
  integer, dimension(jpi,jpj,jpn) :: btmsk

  ! Test code with array notation used in call to array-valued function
  ! (`ptr_sjk`).
  jn = 2

  z3d(1,:,:) =  ptr_sjk( pvtr(:,:,:), btmsk(:,:,jn)*btm30(:,:) ) 

end program implicit_do
