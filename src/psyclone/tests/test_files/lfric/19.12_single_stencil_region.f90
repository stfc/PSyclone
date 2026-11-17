! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_stencil_region
  ! Description: single region stencil specified in an invoke call.
  use testkern_stencil_region_mod, only: testkern_stencil_region_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1,f2,f3,f4
  integer :: f2_extent=1

  call invoke(                                             &
       testkern_stencil_region_type(f1,f2,f2_extent,f3,f4) &
       )

end program single_stencil_region
