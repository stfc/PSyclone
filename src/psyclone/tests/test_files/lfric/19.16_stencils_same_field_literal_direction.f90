! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multiple_stencils
  ! Description: multiple stencils specified with the same direction
  ! provided in two cases and a different direction in the third.
  ! Note: it is currently not possible to specify kind for an integer
  ! literal stencil depth in a kernel call. This will be enabled when
  ! addressing issue #753.
  use field_mod,                   only: field_type
  use flux_direction_mod,          only: x_direction, y_direction
  use testkern_stencil_xory1d_mod, only: testkern_stencil_xory1d_type

  type(field_type) :: f1, f2, f3, f4

  call invoke(                                                       &
       testkern_stencil_xory1d_type(f1, f2, 2, x_direction, f3, f4), &
       testkern_stencil_xory1d_type(f1, f2, 2, x_direction, f3, f4), &
       testkern_stencil_xory1d_type(f1, f2, 2, y_direction, f3, f4)  &
       )

end program multiple_stencils
