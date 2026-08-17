! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program restrict_prolong_anyd

  ! Description: invoke containing a chain of two restrictions and prolongations
  ! from a fine to a coarse mersh and back. Restrictions operate on
  ! any_discontinuous_space so there should not be any halo exchanges for them.
  use field_mod,               only: field_type
  use restrict_kernel_mod,     only: restrict_kernel_type
  use prolong_test_kernel_mod, only: prolong_test_kernel_type

  implicit none

  type(field_type) :: fld_f, fld_m, fld_c

  call invoke(                                        &
              restrict_kernel_type(fld_m, fld_f),     & ! fine -> medium
              restrict_kernel_type(fld_c, fld_m),     & ! medium -> coarse
              prolong_test_kernel_type(fld_m, fld_c), & ! coarse -> medium
              prolong_test_kernel_type(fld_f, fld_m) )  ! medium -> fine


end program restrict_prolong_anyd
