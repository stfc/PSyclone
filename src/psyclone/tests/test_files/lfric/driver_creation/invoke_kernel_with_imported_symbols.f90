! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! A single program calling two kernels which import non-local symbols. One
! of the kernels will also introduce name clashes with names in the created
! PSy-layer.

program invoke_kernel_with_imported_symbols

  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_import_symbols_mod,  only: testkern_import_symbols_type
  use testkern_import_symbols_name_clash_mod,  only: testkern_import_symbols_name_clash_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a

  call invoke(testkern_import_symbols_type(a, f1, f2, m1, m2))
  call invoke(testkern_import_symbols_name_clash_type(a, f1, f2, m1, m2))

end program invoke_kernel_with_imported_symbols
