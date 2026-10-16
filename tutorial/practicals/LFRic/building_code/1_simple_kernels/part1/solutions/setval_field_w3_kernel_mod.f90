! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! A kernel that assigns a value to a field on a discontinuous function space W3
module setval_field_w3_kernel_mod

  use argument_mod,      only: arg_type,            &
                               GH_FIELD, GH_SCALAR, &
                               GH_REAL,             &
                               GH_WRITE, GH_READ,   &
                               CELL_COLUMN
  use fs_continuity_mod, only: W3
  use constants_mod,     only: r_def, i_def
  use kernel_mod,        only: kernel_type

  implicit none

  private

  !-----------------------------------------------------------------------------
  ! The type declaration for the kernel. Contains the metadata needed by
  ! the PSy layer.
  !-----------------------------------------------------------------------------
  type, public, extends(kernel_type) :: setval_field_w3_kernel_type
    private
    type(arg_type), dimension(2) :: meta_args = (/   &
         arg_type(GH_FIELD,  GH_REAL, GH_WRITE, W3), &
         arg_type(GH_SCALAR, GH_REAL, GH_READ)       &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: setval_field_w3_code
  end type setval_field_w3_kernel_type

  public setval_field_w3_code

  contains

  !> @brief Sets a field on W3 function space to a scalar value
  !> @param[in] nlayers Number of layers
  !> @param[out] field_1_w3 Field to update to a scalar
  !> @param[in] rscalar_2 Value to set the field to
  !> @param[in] ndf_w3 Number of degrees of freedom per cell for the
  !!                   updated field
  !> @param[in] undf_w3 Number of unique degrees of freedom for the
  !!                    updated field
  !> @param[in] map_w3 Dofmap for the cell at the base of the column for
  !!                   the updated field
  subroutine setval_field_w3_code(nlayers, field_1_w3, rscalar_2, &
                                  ndf_w3, undf_w3, map_w3)

    implicit none

    ! Arguments
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    real(kind=r_def), intent(in) :: rscalar_2
    real(kind=r_def), intent(inout), dimension(undf_w3) :: field_1_w3

    ! Internal variables
    integer(kind=i_def) :: k, df

    ! Update field
    do k = 0, nlayers-1
      do df = 1, ndf_w3
        field_1_w3( map_w3(df) + k ) = rscalar_2
      end do
    end do

  end subroutine setval_field_w3_code

end module setval_field_w3_kernel_mod
