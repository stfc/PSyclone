! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program halo_reader

  ! Description: single function specified in an invoke call. On
  ! argument (f2) requires a halo operation as it has a stencil. The
  ! extent of the halo is passed in as an argument.
  use constants_mod,        only: i_def
  use field_mod,            only: field_type
  use testkern_stencil_mod, only: testkern_stencil_type

  implicit none

  type(field_type) :: f1, f2, f3, f4
  integer(i_def)   :: f2_extent = 2

  call invoke(                                          &
       testkern_stencil_type(f1, f2, f2_extent, f3, f4) &
          )

end program halo_reader
