! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_restrict

  ! Description: invoke containing a restriction (map a field
  ! from coarse to fine mesh) and a general-purpose kernel.
  ! This is currently forbidden and so PSyclone should object.
  use field_mod,            only: field_type
  use restrict_kernel_mod,  only: restrict_kernel_type
  use testkern_w2_only_mod, only: testkern_w2_only_type

  implicit none

  type(field_type) :: field1, field2, write_fld

  call invoke( restrict_kernel_type(field1, field2), &
               testkern_w2_only_type(write_fld, field2) )

end program single_invoke_restrict
