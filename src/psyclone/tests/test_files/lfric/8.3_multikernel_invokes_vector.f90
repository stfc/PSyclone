! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_vector

  ! Description: two functions in an invoke with vector field dependencies
  use field_mod,             only: field_type
  use testkern_vector_2_mod, only: testkern_vector_2_type

  implicit none

  type(field_type) :: f1

  call invoke(                           &
       testkern_vector_2_type(f1),       &
       testkern_vector_2_type(f1)        &
          )

end program single_invoke_vector
