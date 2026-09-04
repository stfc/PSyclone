! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> An LFRic algorithm which invokes a 'domain' kernel with arguments
!! with different stencil accesses.
program invoke_test
  use testkern_domain_stencil_mod, only: testkern_domain_stencil_type
  use constants_mod, only: i_def, r_def
  use field_mod, only: field_type
  implicit none
  ! Declare minimal variables used in the invoke call
  type(field_type) :: a, b, c, d
  integer(kind=i_def) :: b_extent, c_extent, d_extent

  call invoke( &
       testkern_domain_stencil_type(a, b, b_extent,           &
                                    c, c_direction, c_extent, &
                                    d, d_extent) )

end program invoke_test
