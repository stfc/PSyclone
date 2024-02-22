! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Author: I. Kavcic, Met Office
!
! -----------------------------------------------------------------------------
! A kernel that adds two fields on any function space (must be the same space)
! and stores the result in the field on a same space
! -----------------------------------------------------------------------------
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
