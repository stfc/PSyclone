! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_kernel_any_dscnt_space_stencil
  ! An example where stencils and any_discontinuous_space are used within
  ! a single kernel. We check when any_discontinuous_space is the same and when
  ! it is different. When it is the same we should have the same stencil dofmap
  ! (as all other stencil information is the same) and when it is different we
  ! should have a different stencil dofmap (as we do not know if they are on the
  ! same space).
  ! This also tests the case where we have different fields with the same and
  ! different any_discontinuous_space names in different kernels.
  use constants_mod, only: i_def
  use field_mod,     only: field_type
  use testkern_same_any_dscnt_space_stencil_mod, &
                     only: testkern_same_any_dscnt_space_stencil_type
  use testkern_different_any_dscnt_space_stencil_mod, &
                     only: testkern_different_any_dscnt_space_stencil_type

  implicit none

  type(field_type)          :: f0, f1, f2, f3, f4, f5
  integer(i_def), parameter :: extent = 3

  ! 1) same kernel, different field, same any_discontinuous_space (f1, f2)
  ! 2) same kernel, different field, different any_discontinuous_space (f4, f5)
  ! 3) different kernel, different field, same any_discontinuous_space (f1, f4)
  ! 4) different kernel, different field, different any_discontinuous_space (f2, f5)

  call invoke(                                                     &
       testkern_same_any_dscnt_space_stencil_type(f0,              &
                                                  f1, extent,      &
                                                  f2, extent),     &
       testkern_different_any_dscnt_space_stencil_type(f3,         &
                                                       f4, extent, &
                                                       f5, extent) &
       )

end program single_kernel_any_dscnt_space_stencil
