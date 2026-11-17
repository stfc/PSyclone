! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! This code demonstrates an implicit loop (as the second dimension of
! umask and vmask are specified with a ':') with one or more (4 in this
! case) of the associated array dimensions being single valued.

program implicit_single_value
  implicit none
  integer :: jpi, jpj, jpk, jpt, ndim
  real, dimension(jpi,jpj,jpk,jpt,ndim) :: umask, vmask

  umask(1,:,jpk,jpt,1) = vmask(1,:,jpk,jpt,1) + 1.0

end program implicit_single_value
