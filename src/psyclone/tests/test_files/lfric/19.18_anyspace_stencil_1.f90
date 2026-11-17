! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_kernel_anyspace_stencil
  ! Description: an example where stencils and any_space are used
  ! within a single kernel. We check when any_space is the same and
  ! when it is different. When it is the same we should have the same
  ! stencil dofmap (as all other stencil information is the same) and
  ! when it is different we should have a different stencil dofmap (as
  ! we do not know if they are on the same space). This also tests the
  ! case where we have different fields with the same and different
  ! any_space names in different kernels.
  use constants_mod,                           only: i_def
  use field_mod,                               only: field_type
  use testkern_same_anyspace_stencil_mod,      only: testkern_same_anyspace_stencil_type
  use testkern_different_anyspace_stencil_mod, only: testkern_different_anyspace_stencil_type

  implicit none

  type(field_type) :: f0, f1, f2, f3, f4, f5
  integer(i_def)   :: extent = 2

  ! 1) Same kernel, different field, same any_space (f1, f2)
  ! 2) Same kernel, different field, different any_space (f4, f5)
  ! 3) Different kernel, different field, same any_space (f1, f4)
  ! 4) Different kernel, different field, different any_space (f2, f5)
  call invoke(                                              &
       testkern_same_anyspace_stencil_type(f0,              &
                                           f1, extent,      &
                                           f2, extent),     &
       testkern_different_anyspace_stencil_type(f3,         &
                                                f4, extent, &
                                                f5, extent) &
       )

end program single_kernel_anyspace_stencil
