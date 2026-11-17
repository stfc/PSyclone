! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_restrict

  ! Description: invoke of single kernel that performs a restriction (map
  ! field from coarse to fine mesh) for a field on a continuous space but
  ! with GH_WRITE access.
  use field_mod,              only: field_type
  use restrict_w2_kernel_mod, only: restrict_w2_kernel_type

  implicit none

  type(field_type) :: field1, field2

  call invoke( restrict_w2_kernel_type(field1, field2) )

end program single_invoke_restrict
