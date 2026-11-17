! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program halo_inc_to_readinc

  ! Description: test that a field with 'gh_readinc' access requires a
  ! halo exchange before it (unlike a field with 'gh_inc' access which
  ! does not) where the field is continuous and both loops iterate
  ! over cells.

  use field_mod,               only: field_type
  use testkern_w0_mod,         only: testkern_w0_type
  use testkern_w0_readinc_mod, only: testkern_w0_readinc_type

  implicit none

  type(field_type) :: f1, f2

  call invoke(                          &
       testkern_w0_type(f1, f2),        &
       testkern_w0_readinc_type(f1, f2) &
          )

end program halo_inc_to_readinc
