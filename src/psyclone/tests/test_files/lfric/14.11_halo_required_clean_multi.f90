! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program halo_different_stencils

  ! Description: two stencil accesses in different kernels associated
  ! with the same field and therefore halo exchange when distributed
  ! memory is used. The stencils are of different types so region is
  ! returned to ensure that both stencil accesses are covered. This
  ! could be improved by noticing particular cases e.g. stencil_y +
  ! stencil_cross stays as stencil_cross (which would happen in this
  ! example). However, the halo exchange library does make use of this
  ! information at the moment in any case.
  ! Note: it is currently not possible to specify kind for an integer
  ! literal stencil depth in a kernel call. This will be enabled when
  ! addressing issue #753.
  use constants_mod,                  only: i_def, r_def
  use field_mod,                      only: field_type
  use flux_direction_mod,             only: y_direction
  use testkern_stencil_w3_mod,        only: testkern_stencil_w3_type
  use testkern_stencil_xory1d_w3_mod, only: testkern_stencil_xory1d_w3_type

  implicit none

  type(field_type) :: f1, f2, f3
  integer(i_def)   :: f2_extent = 2
  integer(i_def)   :: f2_direction = y_direction

  call invoke(                                                  &
       setval_c(f2, 0.0_r_def),                                 &
       ! f1 is w3 and is written to
       ! f2 is w2 and is read with stencil cross
       testkern_stencil_w3_type(f1, f2, f2_extent),             &
       ! f3 is w3 and is written to
       ! f2 is w2 and is read with stencil xory1d
       testkern_stencil_xory1d_w3_type(f3, f2, 2, f2_direction) &
          )

end program halo_different_stencils
