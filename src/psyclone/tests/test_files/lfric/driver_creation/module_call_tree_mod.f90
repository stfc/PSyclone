! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2023-2026, Science and Technology Facilities Council.
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Author: J. Henrichs, Bureau of Meteorology

! This test program contains all test cases of subroutines, programs and
! functions using different non-local symbols for testing the call tree
! functionality, but no actual kernel calls. Full details in:
! call_tree_utils_test.test_call_tree_compute_all_non_locals_non_kernel()

module module_call_tree_mod

  integer, parameter :: const=2
  integer :: module_var
  real :: module_var_real
  double precision :: module_var_double

  ! Test for generic interface. Note that integer_func is not implemented,
  ! this will cause the exception handling in call_tree_utils to be tested.
  interface generic_function
    module procedure real_func, double_func, integer_func
  end interface

  public :: call_generic_function

contains

  function real_func(a) result(a_new)
    implicit none
    real :: a, a_new
    a_new = a+module_var_real
  end function real_func

  function double_func(a) result(a_new)
    implicit none
    double precision :: a, a_new
    a_new = a+module_var_double
  end function double_func

  subroutine call_generic_function(a)
    real :: a
    a = generic_function(a)
  end subroutine call_generic_function

  subroutine local_var_sub()
    integer :: i
    integer, save :: saved_variable
    i = 1
  end subroutine local_var_sub

  subroutine module_var_sub()
    module_var = 1
  end subroutine module_var_sub

  subroutine call_local_function()
    integer i
    i = module_function(1)
  end subroutine call_local_function

  subroutine local_const_sub()
    integer :: i
    i = const
  end subroutine local_const_sub

  subroutine argument_sub(arg)
    integer, intent(out) :: arg
    arg = 1
  end subroutine argument_sub

  integer function module_function(a)
    integer :: a
    module_function = 1
  end function module_function

  subroutine calling_unknown_subroutine(arg)
    integer, intent(out) :: arg
    call unknown_subroutine()
  end subroutine calling_unknown_subroutine

  subroutine calling_imported_subroutine()
    use some_module, only: module_subroutine
    call module_subroutine()
  end subroutine calling_imported_subroutine

  subroutine use_imported_symbol()
    use some_module1, only: module_var1
    use some_module2, only: var2 =>module_var2
    real :: r
    r = module_var1 + var2
  end subroutine use_imported_symbol

  subroutine intrinsic_call()
    real :: r
    r = sqrt(1.0)
  end subroutine intrinsic_call


end module module_call_tree_mod

program main
  use module_call_tree_mod

  call call_local_function()
end program main