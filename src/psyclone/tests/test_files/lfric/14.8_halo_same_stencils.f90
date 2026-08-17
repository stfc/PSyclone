! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program halo_same_stencils

  ! Description: two stencil accesses in different kernels associated
  ! with the same field ('f2') and therefore halo exchange when
  ! distributed memory is used. The stencils are of the same type
  ! ('cross' in this case) so 'cross' is returned which will ensure
  ! that both stencil accesses are covered.
  use constants_mod,           only: i_def, r_def
  use field_mod,               only: field_type
  use testkern_stencil_w3_mod, only: testkern_stencil_w3_type

  implicit none

  type(field_type) :: f1, f2, f3
  integer(i_def)   :: f2_extent = 2

  call invoke(                                      &
       setval_c(f2, 0.0_r_def),                     &
       ! f1 is w3 and is written to
       ! f2 is w2 and is read with stencil cross
       ! f3 is w3 and is written to
       testkern_stencil_w3_type(f1, f2, f2_extent), &
       testkern_stencil_w3_type(f3, f2, f2_extent)  &
          )

end program halo_same_stencils
