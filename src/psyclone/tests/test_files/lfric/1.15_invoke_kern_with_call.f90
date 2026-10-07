! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single kernel specified in an invoke call where the kernel
  ! itself contains a call.
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_with_call_mod,  only: testkern_with_call_type

  implicit none

  type(field_type) :: geopotential
  type(field_type), dimension(3) :: chi
  real(r_def)      :: gravity = 9.8
  real(r_def)      :: scaled_radius = 6.4E6

  call invoke( testkern_with_call_type( geopotential,  &
                                        chi,           &
                                        gravity,       &
                                        scaled_radius ) )

end program single_invoke
