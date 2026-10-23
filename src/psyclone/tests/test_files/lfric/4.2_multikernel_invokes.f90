! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_3

  ! Multiple kernel calls within an invoke where the kernels require
  ! a quadrature rule

  use field_mod,             only: field_type
  use testkern_coord_w0_mod, only: testkern_coord_w0_type

  implicit none

  type(field_type) :: f1, chi(3), f2

  call invoke(                              &
       testkern_coord_w0_type(f1, chi ,f2), &
       testkern_coord_w0_type(f1, chi ,f2)  &
       )

end program multikernel_invokes_3
