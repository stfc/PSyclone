! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! A simple module that calls a kernel and also uses a builtin. This is used
! for testing the basic functionality of extraction and driver creation

module module_with_builtin

contains

  subroutine sub_with_builtin
    use constants_mod, only: r_def
    use field_mod,     only: field_type
    use testkern_import_symbols_mod,  only: testkern_import_symbols_type

    implicit none

    type(field_type) :: f1, f2, m1, m2
    real(r_def)      :: a

    call invoke(testkern_import_symbols_type(a, f1, f2, m1, m2), &
                setval_c(f1, 1.0))

  end subroutine sub_with_builtin

end module module_with_builtin
