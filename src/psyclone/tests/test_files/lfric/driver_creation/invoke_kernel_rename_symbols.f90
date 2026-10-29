! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program invoke_kernel_with_imported_symbols

  ! Description: The kernel will call a subroutine which renames a symbol
  ! it imports. This is used to test the handling of non-local renamed
  ! variables in extraction and driver creation.
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_rename_symbols_mod,  only: testkern_rename_symbols_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: module_var_a

  call invoke(testkern_rename_symbols_type(module_var_a, f1, f2, m1, m2))

end program invoke_kernel_with_imported_symbols
