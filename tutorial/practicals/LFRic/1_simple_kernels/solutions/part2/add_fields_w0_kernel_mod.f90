! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2020, Science and Technology Facilities Council.
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
! A kernel that adds two fields on a continuous function space W0 and stores
! the result in the third field on the same space
! -----------------------------------------------------------------------------
module add_fields_w0_kernel_mod

  use argument_mod,      only: arg_type, GH_FIELD, &
                               GH_INC, GH_READ, CELLS
  use fs_continuity_mod, only: W0
  use constants_mod,     only: r_def, i_def
  use kernel_mod,        only: kernel_type

  implicit none

  private

  !-----------------------------------------------------------------------------
  ! The type declaration for the kernel. Contains the metadata needed by
  ! the PSy layer.
  !-----------------------------------------------------------------------------
  type, public, extends(kernel_type) :: add_fields_w0_kernel_type
    private
    type(arg_type), dimension(3) :: meta_args = (/ &
         arg_type(GH_FIELD, GH_INC,  W0),          &
         arg_type(GH_FIELD, GH_READ, W0),          &
         arg_type(GH_FIELD, GH_READ, W0)           &
         /)
    integer :: iterates_over = CELLS
  contains
    procedure, nopass :: add_fields_w0_code
  end type add_fields_w0_kernel_type

  public add_fields_w0_code

  contains

  !> @brief Adds two fields on W0 function space
  !> @param[in] nlayers Number of layers
  !> @param[in,out] field_1_w0 Resulting field
  !> @param[in] field_2_w0 First field to add
  !> @param[in] field_3_w0 Second field to add
  !> @param[in] ndf_w0 Number of degrees of freedom per cell for the
  !!                   updated field
  !> @param[in] undf_w0 Number of unique degrees of freedom for the
  !!                    updated field
  !> @param[in] map_w0 Dofmap for the cell at the base of the column for
  !!                   the updated field
  subroutine add_fields_w0_code(nlayers, field_1_w0,    &
                                field_2_w0, field_3_w0, &
                                ndf_w0, undf_w0, map_w0)

    implicit none

    ! Arguments
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: undf_w0
    integer(kind=i_def), intent(in), dimension(ndf_w0) :: map_w0
    real(kind=r_def), intent(inout), dimension(undf_w0) :: field_1_w0
    real(kind=r_def), intent(in),    dimension(undf_w0) :: field_2_w0
    real(kind=r_def), intent(in),    dimension(undf_w0) :: field_3_w0

    ! Internal variables
    integer(kind=i_def) :: k, df

    ! Update field
    do k = 0, nlayers-1
      do df = 1, ndf_w0
        field_1_w0( map_w0(df) + k ) = &
          field_2_w0( map_w0(df) + k ) + field_3_w0( map_w0(df) + k )
      end do
    end do

  end subroutine add_fields_w0_code

end module add_fields_w0_kernel_mod
