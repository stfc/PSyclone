! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! A single program calling a kernel which imports symbols from a module that
! cannot be parsed. Used for testing error handling.

program invoke_kernel_with_imported_symbols_error

  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_import_symbols_error_mod,  only: testkern_import_symbols_error_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a

  call invoke(testkern_import_symbols_error_type(a, f1, f2, m1, m2))

end program invoke_kernel_with_imported_symbols_error
