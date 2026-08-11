! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! This kernel uses a function, subroutine, constant and variables from this
! module. It is used to check that kernel extraction and driver creation
! will correctly these usages of module symbols as external to the
! PSy-layer.

module testkern_import_symbols_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  integer :: dummy_module_variable
  integer, parameter :: dummy_constant=2

  type, extends(kernel_type) :: testkern_import_symbols_type
     type(arg_type), dimension(5) :: meta_args =        &
          (/ arg_type(gh_scalar, gh_real, gh_read),     &
             arg_type(gh_field,  gh_real, gh_inc,  w1), &
             arg_type(gh_field,  gh_real, gh_read, w2), &
             arg_type(gh_field,  gh_real, gh_read, w2), &
             arg_type(gh_field,  gh_real, gh_read, w3)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_import_symbols_code
  end type testkern_import_symbols_type

contains

  subroutine local_subroutine()
  end subroutine local_subroutine

  integer function local_func(arg)
  integer :: arg
  local_func = 1
  end function

  subroutine testkern_import_symbols_code(nlayers, ascalar,        &
                                          fld1, fld2, fld3, fld4,  &
                                          ndf_w1, undf_w1, map_w1, &
                                          ndf_w2, undf_w2, map_w2, &
                                          ndf_w3, undf_w3, map_w3)
    use constants_mod, only: eps, i_def, r_def
    use module_with_var_mod, only: module_subroutine, module_var_a, &
                                   module_function, module_const

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w1, undf_w2, undf_w3
    integer(kind=i_def), intent(in), dimension(ndf_w1) :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    real(kind=r_def), intent(in) :: ascalar
    real(kind=r_def), intent(inout), dimension(undf_w1) :: fld1
    real(kind=r_def), intent(in), dimension(undf_w2)  :: fld2
    real(kind=r_def), intent(in), dimension(undf_w2)  :: fld3
    real(kind=r_def), intent(in), dimension(undf_w3)  :: fld4
    real(kind=r_def) :: tmp

    ! Also test handling of intrinsics
    tmp = fld2(1)*fld3(1)*fld4(1) + sqrt(ascalar)
    fld1(1) = eps * nlayers + tmp
    dummy_module_variable = 1 + dummy_constant + local_func(1) + module_const
    fld1(1) = module_function()
    call module_subroutine()
    call local_subroutine()
    call unknown_subroutine()
    module_var_a = 1

  end subroutine testkern_import_symbols_code

end module testkern_import_symbols_mod
