! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program halo_writers

  ! Description: two kernels that update fields on W2. The first has a 'GH_WRITE'
  ! access for a field (plus stencil accesses on two other fields) and the second
  ! is a more typical kernel that has a 'GH_INC' access for its updated field.
  use field_mod,                     only: field_type
  use testkern_write_w2_stencil_mod, only: testkern_write_w2_stencil_type
  use testkern_anyw2_stencil_mod,    only: testkern_anyw2_stencil_type
  implicit none

  type(field_type) :: f1, f2, f3, f4
  integer :: depth = 1
  real(kind=r_def) :: a = 0.5_r_def

  call invoke( testkern_write_w2_stencil_type(f1, f2, depth, f3, depth, a), &
               testkern_anyw2_stencil_type(f1, f2, depth, f3, depth) )

end program halo_writers
