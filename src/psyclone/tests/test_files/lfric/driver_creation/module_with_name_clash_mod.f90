! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! This module declares some variable names that will clash with names used
! in the PSy layer. It is used by some kernels which allows testing the
! name clash handling.

module module_with_name_clash_mod

  integer :: module_var_a, f1_data, f2_data

contains

  subroutine module_function()
    f2_data = f2_data + 1
  end subroutine module_function


end module module_with_name_clash_mod
