! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program vector_field

  ! Description: vector field obtained by de-referencing a derived type
  ! passed as an argument
  use field_mod,             only: field_type, field_container_type
  use testkern_coord_w0_mod, only: testkern_coord_w0_type

  implicit none

  type(field_type)           :: f1, f2
  type(field_container_type) :: box

  call invoke( testkern_coord_w0_type(f1, box%chi, f2) )

end program vector_field
