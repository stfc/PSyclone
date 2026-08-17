! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program stencil_then_read

  ! Test that a discontinuous stencil 'read' (which happens to be
  ! 'any_discontinuous_space' but does not matter) followed by a
  ! discontinuous read (which happens to be 'readwrite' but does not
  ! matter) results in correct halo exchange declarations. The example
  ! is field f4. There was a bug here before this test and fix was
  ! added.

  use constants_mod, only: r_def
  use field_mod,     only: field_type

  use testkern_different_any_dscnt_space_stencil_mod, only : &
                     testkern_different_any_dscnt_space_stencil_type
  use testkern_w3_mod, only : testkern_w3_type

  type(field_type) :: f1, f2, f3, f4
  real(r_def)      :: a, extent

  call invoke(                                                     &
      testkern_different_any_dscnt_space_stencil_type(f1,          &
                                                      f2, extent,  &
                                                      f4, extent), &
      testkern_w3_type(a, f1, f2, f3, f4)                          &
      )

end program stencil_then_read
