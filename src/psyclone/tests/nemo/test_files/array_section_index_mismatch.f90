! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program implicit_do
  implicit none
  integer, parameter :: jpi=10, jpj=10, jpk=10, jpn=2
  integer :: ji, jk, jn, jp_tem
  real(kind=kind(1.0d0)), dimension(jpi,jpj,jpk,jpn) :: zab
  real(kind=kind(1.0d0)), dimension(jpk, jpn) :: zvab

  ! Test code where array notation occurs in different dimensions of the
  ! various arrays in an assignment.
  jp_tem = 2

  zvab(:,jp_tem) = zab(ji,jj,:,jp_tem)

  zab(ji,jj,:,1) = zvab(:,jj)

end program implicit_do
