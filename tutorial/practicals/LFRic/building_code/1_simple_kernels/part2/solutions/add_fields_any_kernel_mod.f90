! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! A kernel that adds two fields on any function space (must be the same space)
! and stores the result in the field on a same space
module add_fields_any_kernel_mod

  use argument_mod,      only: arg_type,          &
                               GH_FIELD, GH_REAL, &
                               GH_INC, GH_READ,   &
                               ANY_SPACE_1, CELL_COLUMN
  use constants_mod,     only: r_def, i_def
  use kernel_mod,        only: kernel_type

  implicit none

  private

  !-----------------------------------------------------------------------------
  ! The type declaration for the kernel. Contains the metadata needed by
  ! the PSy layer.
  !-----------------------------------------------------------------------------
  type, public, extends(kernel_type) :: add_fields_any_kernel_type
    private
    type(arg_type), dimension(3) :: meta_args = (/          &
         arg_type(GH_FIELD, GH_REAL, GH_INC,  ANY_SPACE_1), &
         arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_1), &
         arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_1)  &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: add_fields_any_code
  end type add_fields_any_kernel_type

  public add_fields_any_code

  contains

  !> @brief Adds two fields on any function space
  !> @param[in] nlayers Number of layers
  !> @param[in,out] field_1_aspc1_field_1 Resulting field
  !> @param[in] field_2_aspc1_field_1 First field to add
  !> @param[in] field_3_aspc1_field_1 Second field to add
  !> @param[in] ndf_aspc1_field_1 Number of degrees of freedom per cell
  !!                              for the updated field
  !> @param[in] undf_aspc1_field_1 Number of unique degrees of freedom
  !!                               for the updated field
  !> @param[in] map_aspc1_field_1 Dofmap for the cell at the base of the
  !!                              column for the updated field
  subroutine add_fields_any_code(nlayers, field_1_aspc1_field_1,               &
                                 field_2_aspc1_field_1, field_3_aspc1_field_1, &
                                 ndf_aspc1_field_1, undf_aspc1_field_1, map_aspc1_field_1)

    implicit none

    ! Arguments
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_aspc1_field_1
    integer(kind=i_def), intent(in), dimension(ndf_aspc1_field_1) :: map_aspc1_field_1
    integer(kind=i_def), intent(in) :: undf_aspc1_field_1
    real(kind=r_def), intent(inout), dimension(undf_aspc1_field_1) :: field_1_aspc1_field_1
    real(kind=r_def), intent(in), dimension(undf_aspc1_field_1) :: field_2_aspc1_field_1
    real(kind=r_def), intent(in), dimension(undf_aspc1_field_1) :: field_3_aspc1_field_1

    ! Internal variables
    integer(kind=i_def) :: k, df

    ! Update field
    do k = 0, nlayers-1
      do df = 1, ndf_aspc1_field_1
        field_1_aspc1_field_1( map_aspc1_field_1(df) + k ) =   &
          field_2_aspc1_field_1( map_aspc1_field_1(df) + k ) + &
          field_3_aspc1_field_1( map_aspc1_field_1(df) + k )
      end do
    end do

  end subroutine add_fields_any_code

end module add_fields_any_kernel_mod
