! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! This module provides some module variables and a function/subroutine to
! provide non-local symbols for testing kernel extraction and driver creation.

module module_with_var_mod

  integer :: module_var_a, module_var_b
  integer, parameter :: module_const = 123
  real, dimension(100) :: const_size_array

contains

  integer function module_function()
    module_function = module_var_b
  end function module_function

  subroutine module_subroutine()
    module_var_b = module_var_b + 1
    const_size_array (module_var_b) = const_size_array(module_var_b) + 1
  end subroutine module_subroutine


end module module_with_var_mod
