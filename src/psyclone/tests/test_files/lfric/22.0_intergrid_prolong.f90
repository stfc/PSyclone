! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_prolong

  ! Description: invoke of single kernel that performs a prolongation
  use field_mod,               only: field_type
  use prolong_test_kernel_mod, only: prolong_test_kernel_type

  implicit none

  type(field_type) :: field1, field2

  call invoke( prolong_test_kernel_type(field1, field2) )

end program single_invoke_prolong
