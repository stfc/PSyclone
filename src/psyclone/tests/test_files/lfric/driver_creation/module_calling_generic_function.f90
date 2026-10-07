! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! A simple module that calls a generic function in
! module_calling_generic_function.f90. This is used to test the call tree
! tools when using generic functions.

module module_calling_generic_function

contains

  subroutine calling_generic_function
    use module_call_tree_mod, only: generic_function 

    implicit none

    real :: r
    r = generic_function(r)

  end subroutine calling_generic_function

end module module_calling_generic_function
