! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_stencil
  ! Description: single stencil specified in an invoke call with a
  ! literal value passed for the extent value
  ! Note: it is currently not possible to specify kind for an integer
  ! literal stencil depth in a kernel call. This will be enabled when
  ! addressing issue #753.
  use field_mod,            only: field_type
  use testkern_stencil_mod, only: testkern_stencil_type

  implicit none

  type(field_type) :: f1, f2, f3, f4

  call invoke(                                  &
       testkern_stencil_type(f1, f2, 1, f3, f4) &
       )

end program single_stencil
