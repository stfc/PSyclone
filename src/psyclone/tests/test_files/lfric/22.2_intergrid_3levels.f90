! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program restrict_prolong

  ! Description: invoke containing restrictions/prolongations where
  ! fields swap roles (what was 'fine' becomes 'coarse'). Once of the fields is
  ! also named so as to provoke a name clash with the standard name that
  ! PSyclone would generate for a colour map.
  use field_mod,                only: field_type
  use restrict_test_kernel_mod, only: restrict_test_kernel_type
  use prolong_test_kernel_mod,  only: prolong_test_kernel_type

  implicit none

  type(field_type) :: fld_f, fld_m, cmap_fld_c

  call invoke(                                             &
              prolong_test_kernel_type(fld_m, cmap_fld_c), & ! coarse -> medium
              prolong_test_kernel_type(fld_f, fld_m),      & ! medium -> fine
              restrict_test_kernel_type(fld_m, fld_f),     & ! fine -> medium
              restrict_test_kernel_type(cmap_fld_c, fld_m) ) ! medium -> coarse

end program restrict_prolong
