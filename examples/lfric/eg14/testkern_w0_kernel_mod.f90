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

  implicit none

  private

  type, public, extends(kernel_type) :: testkern_w0_kernel_type
     private
     type(arg_type), dimension(2) :: meta_args =       &
          (/ arg_type(gh_field, gh_real, gh_inc,  w0), &
             arg_type(gh_field, gh_real, gh_read, w0)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_w0_code
  end type testkern_w0_kernel_type

  public :: testkern_w0_code

contains

  subroutine testkern_w0_code(nlayers, fld1, fld2, ndf_w0, undf_w0, map_w0)

    implicit none

    integer(kind=i_def), intent(in)                     :: nlayers
    integer(kind=i_def), intent(in)                     :: ndf_w0, undf_w0
    real(kind=r_def), dimension(undf_w0), intent(inout) :: fld1
    real(kind=r_def), dimension(undf_w0), intent(in)    :: fld2
    integer(kind=i_def), dimension(ndf_w0), intent(in)  :: map_w0

    integer(kind=i_def)                                 :: i, k

    do k=0, nlayers-1
      do i=1, ndf_w0
        fld1(map_w0(i)+k) = fld1(map_w0(i)+k) + fld2(map_w0(i)+k)
      end do
    end do

  end subroutine testkern_w0_code

end module testkern_w0_kernel_mod
