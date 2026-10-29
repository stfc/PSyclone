! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program halo_depth

  use constants_mod,              only: i_def
  use field_mod,                  only: field_type
  use testkern_stencil_depth_mod, only: testkern_stencil_depth_type

  implicit none
  type(field_type) :: f1, f2, f3, f4
  integer(i_def)   :: extent = 2

  call invoke(                                                &
       testkern_stencil_depth_type(f1, f2, extent, f3,extent, &
                                   f4, extent)                &
          )

end program halo_depth
