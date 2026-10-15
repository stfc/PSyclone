! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multi_kernel_anyspace_stencil

  ! Description: an example where stencils and any_space are used
  ! in different kernels. We check that the same field always has the
  ! same stencil dofmap irrespective of the any_space name.
  use constants_mod,                           only: i_def
  use field_mod,                               only: field_type
  use testkern_same_anyspace_stencil_mod,      only: testkern_same_anyspace_stencil_type
  use testkern_different_anyspace_stencil_mod, only: testkern_different_anyspace_stencil_type

  implicit none

  type(field_type) :: f0, f1, f2, f3
  integer(i_def)   :: extent = 2

  ! 1) Different kernel, same field, same any_space (f1)
  ! 2) Different kernel, same field, different any_space (f2)
  call invoke(                                              &
       testkern_same_anyspace_stencil_type(f0,              &
                                           f1, extent,      &
                                           f2, extent),     &
       testkern_different_anyspace_stencil_type(f3,         &
                                                f1, extent, &
                                                f2, extent) &
       )

end program multi_kernel_anyspace_stencil
