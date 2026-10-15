! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program halo_reader_vector_xory

  ! Test halo exchange calls, stencils and vector fields.
  use constants_mod,                    only: i_def
  use field_mod,                        only: field_type
  use flux_direction_mod,               only: x_direction
  use testkern_stencil_vector_xory_mod, only: testkern_stencil_vector_xory_type

  implicit none

  type(field_type) :: f1, f2
  integer(i_def)   :: f2_extent = 3
  integer(i_def)   :: f2_direction = x_direction

  call invoke(                                                            &
       testkern_stencil_vector_xory_type(f1, f2, f2_extent, f2_direction) &
          )

end program halo_reader_vector_xory
