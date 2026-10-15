! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program vector_field

  ! Description: vector field passed as an argument
  use field_mod,               only: field_type
  use testkern_coord_w0_2_mod, only: testkern_coord_w0_2_type

  implicit none

  type(field_type) :: f1, chi(3)

  call invoke(                           &
       testkern_coord_w0_2_type(chi, f1) &
       )

end program vector_field
