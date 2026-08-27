! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> An LFRic algorithm which invokes several, different 'domain' kernels, each
!! of which requires a different stencil type.
program invoke_test
  use testkern_domain_stencil_mod, only: testkern_domain_stencil_type
  use testkern_domain_stencil_unknown_extent_mod, only: &
       testkern_domain_stencil_unknown_extent_type
  use testkern_domain_stencil_unknown_extent2d_mod, only: &
       testkern_domain_stencil_unknown_extent2d_type
  use constants_mod, only: i_def, r_def
  use field_mod, only: field_type
  implicit none
  ! Declare minimal variables used in the invoke call
  type(field_type) :: a, b, c, d
  integer(kind=i_def) :: b_extent

  call invoke( testkern_domain_stencil_type(a, b, b_extent, c), &
       testkern_domain_stencil_unknown_extent_type(), &
       testkern_domain_stencil_unknown_extent2d_type() )

end program invoke_test
