! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! A kernel that tries to import a two symbols from a file that is incorrect
! Fortran. This kernel is used to test error handling in kernel extraction
! and driver creation.

module testkern_import_symbols_error_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  type, extends(kernel_type) :: testkern_import_symbols_error_type
     type(arg_type), dimension(5) :: meta_args =        &
          (/ arg_type(gh_scalar, gh_real, gh_read),     &
             arg_type(gh_field,  gh_real, gh_inc,  w1), &
             arg_type(gh_field,  gh_real, gh_read, w2), &
             arg_type(gh_field,  gh_real, gh_read, w2), &
             arg_type(gh_field,  gh_real, gh_read, w3)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_import_symbols_error_code
  end type testkern_import_symbols_error_type

contains

  subroutine testkern_import_symbols_error_code(nlayers, ascalar,        &
                                          fld1, fld2, fld3, fld4,  &
                                          ndf_w1, undf_w1, map_w1, &
                                          ndf_w2, undf_w2, map_w2, &
                                          ndf_w3, undf_w3, map_w3)
    use constants_mod, only: eps, i_def, r_def
    use module_with_error_mod, only: non_existent_func, non_existent_var
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

    tmp = fld2(1)*fld3(1)*fld4(1)
    fld1(1) = eps * nlayers + tmp + non_existent_var
    call non_existent_func()

  end subroutine testkern_import_symbols_error_code

end module testkern_import_symbols_error_mod
