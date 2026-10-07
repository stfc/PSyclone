! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------


module testkern_w0_kernel_mod

  use argument_mod
  use kernel_mod
  use fs_continuity_mod, only: W0

  use constants_mod

  ! This is used to showcase the ability of the kernel extraction
  ! to write and for the driver creation to read non-local module variables
  ! when importing them in the module scope
  use dummy_mod, only: dummy_var1, dummy_code

  implicit none

  integer, public :: some_other_var
  integer, parameter :: some_other_const = 123
  private

  type, public, extends(kernel_type) :: testkern_w0_kernel_type
     private
     type(arg_type), dimension(4) :: meta_args =       &
          (/ arg_type(gh_field, gh_real, gh_inc,  w0), &
             arg_type(gh_field, gh_real, gh_read, w0), &
             arg_type(gh_field*3, gh_real, gh_read, w0), &
             arg_type(gh_scalar, gh_logical, gh_read)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_w0_code
  end type testkern_w0_kernel_type

  public :: testkern_w0_code

contains

  subroutine testkern_w0_code(nlayers, fld1, fld2, chi1, chi2, chi3, &
                              some_logical, ndf_w0, undf_w0, map_w0)

    ! This is used to showcase the ability of the kernel extraction
    ! to write and for the driver creation to read non-local module
    ! variables when importing them in the kernel itself.
    use dummy_mod, only: dummy_var2, dummy_var3, dummy_func, dummy_code
    implicit none

    integer(kind=i_def), intent(in)                     :: nlayers
    integer(kind=i_def)                                 :: ndf_w0, undf_w0
    real(kind=r_def), dimension(undf_w0), intent(inout) :: fld1
    real(kind=r_def), dimension(undf_w0), intent(in)    :: fld2
    real(kind=r_def), dimension(undf_w0), intent(in)    :: chi1,chi2,chi3
    logical(kind=l_def), intent(in)                     :: some_logical
    integer(kind=i_def), dimension(ndf_w0)              :: map_w0

    integer(kind=i_def)                                 :: i, k
    real(kind=r_def) :: some_r

    call dummy_code(1)
    some_r = 0
    do k=0, nlayers-1
      do i=1, ndf_w0
        some_r = some_r + 1
        fld1(map_w0(i)+k) = fld1(map_w0(i)+k) + fld2(map_w0(i)+k)             &
                          + dummy_func(i)
        if (some_logical) then
          fld1(map_w0(i)+k) = fld1(map_w0(i)+k) + 1 + dummy_var1 + dummy_var2 &
                            + some_other_var + some_r + dummy_var3            &
                            + some_other_const
        endif
      end do
    end do

  end subroutine testkern_w0_code

end module testkern_w0_kernel_mod
