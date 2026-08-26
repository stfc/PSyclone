! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! This modules renames a variable it imports. This is used to test the
! handling of renaming of non-local variables in extraction and
! driver creation.

module module_renaming_external_var_mod

contains

  subroutine renaming_subroutine(a)
    use module_with_var_mod, only: renamed_var => module_var_a
    integer :: a
    a = renamed_var
  end subroutine renaming_subroutine

end module module_renaming_external_var_mod
