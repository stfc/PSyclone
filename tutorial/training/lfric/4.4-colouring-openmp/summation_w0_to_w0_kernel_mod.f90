! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module summation_w0_to_w0_kernel_mod

  use argument_mod
  use kernel_mod
  use fs_continuity_mod, only: W0

  use constants_mod

  implicit none

  private

  type, public, extends(kernel_type) :: summation_w0_to_w0_kernel_type
     private
     type(arg_type), dimension(2) :: meta_args =       &
          (/ arg_type(gh_field, gh_real, gh_inc,  w0), &
             arg_type(gh_field, gh_real, gh_read, w0)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => summation_w0_to_w0_code
  end type summation_w0_to_w0_kernel_type

  public :: summation_w0_to_w0_code

contains

  !> This kernel adds the values from the second field to the first field.
  !> Note that each of these summation can be executed more than once: a vertex
  !> is shared among up to 8 finite elements. So, a vertex value (i.e. on W0)
  !> will be summed up to 8 times (if the vertex has 8 3D-finite-elements).
  !>
  !> @param[in] nlayers Integer the number of layers.
  !> @param[in,out] fld1 The field on W0 to store the sum.
  !> @param[in] fld2 The field on W0 which is added.
  !> @param[in] ndf_w0 Integer the number of degrees of freedom in W0
  !> @param[in] undf_w0 Integer value indicating the number of unique degrees
  !>                    of freedom in W0
  !> @param[in] map_w0 Integer array holding the dofmap for the cell at the
  !>                   base of the column for W0
  subroutine summation_w0_to_w0_code(nlayers, fld1, fld2, ndf_w0, undf_w0, map_w0)

    implicit none

    integer(kind=i_def), intent(in)                     :: nlayers
    integer(kind=i_def)                                 :: ndf_w0, undf_w0
    real(kind=r_def), dimension(undf_w0), intent(inout) :: fld1
    real(kind=r_def), dimension(undf_w0), intent(in)    :: fld2
    integer(kind=i_def), dimension(ndf_w0)              :: map_w0

    integer(kind=i_def)                                 :: i, k

    do k=0, nlayers-1
      do i=1, ndf_w0
        fld1(map_w0(i)+k) = fld1(map_w0(i)+k) + fld2(map_w0(i)+k)
      end do
    end do

  end subroutine summation_w0_to_w0_code

end module summation_w0_to_w0_kernel_mod
