! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program halo_reader_fs

  ! A single kernel call testing all function spaces. Each function
  ! space has a stencil operation so that it requires halo exchange
  ! calls. All extents are passed in and are the same for all fields.
  use constants_mod,           only: i_def
  use field_mod,               only: field_type
  use testkern_stencil_fs_mod, only: testkern_stencil_fs_type

  implicit none

  type(field_type)    :: f1, f2, f3, f4, f5, f6, f7, f8, &
                         f9, f10, f11, f12, f13, f14, f15, f16
  integer(kind=i_def) :: extent = 1

  call invoke(                                            &
       testkern_stencil_fs_type(f1,                       &
                                f2, extent, f3, extent,   &
                                f4, extent, f5, extent,   &
                                f6, extent, f7, extent,   &
                                f8, extent, f9, extent,   &
                                f10, extent, f11, extent, &
                                f12, extent, f13, extent, &
                                f14, extent, f15, extent, &
                                f16, extent) )

end program halo_reader_fs
