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
! A kernel that assigns a value to a field on a discontinuous function space W3
! -----------------------------------------------------------------------------
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
