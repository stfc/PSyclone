! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program halo_reader_vector

  use field_mod,             only: field_type
  use testkern_vector_2_mod, only: testkern_vector_2_type

  implicit none

  type(field_type) :: f1(3)

  call invoke(                         &
       testkern_vector_2_type(f1)      &
          )

end program halo_reader_vector
