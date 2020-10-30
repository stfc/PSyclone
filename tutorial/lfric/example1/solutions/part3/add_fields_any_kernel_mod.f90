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
! A kernel that assigns a value to a field on any function space
! -----------------------------------------------------------------------------
module add_fields_any_kernel_mod

  use argument_mod,      only: arg_type, GH_FIELD, &
                               ANY_SPACE_1,        &
                               GH_INC, GH_READ, CELLS
  use constants_mod,     only: r_def, i_def
  use kernel_mod,        only: kernel_type

  implicit none

  private

  !-----------------------------------------------------------------------------
  ! Public types
  !-----------------------------------------------------------------------------
  type, public, extends(kernel_type) :: add_fields_any_kernel_type
    private
    type(arg_type), dimension(3) :: meta_args = (/ &
         arg_type(GH_FIELD, GH_INC,  ANY_SPACE_1), &
         arg_type(GH_FIELD, GH_READ, ANY_SPACE_1), &
         arg_type(GH_FIELD, GH_READ, ANY_SPACE_1)  &
         /)
    integer :: iterates_over = CELLS
  contains
    procedure, nopass :: add_fields_any_code
  end type add_fields_any_kernel_type

  !-----------------------------------------------------------------------------
  ! Contained functions/subroutines
  !-----------------------------------------------------------------------------
  public add_fields_any_code

  contains

  subroutine add_fields_any_code(nlayers, field_1_aspc1,       &
                                 field_2_aspc1, field_3_aspc1, &
                                 ndf_aspc1, undf_aspc1, map_aspc1)

    implicit none

    ! Arguments
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_aspc1
    integer(kind=i_def), intent(in) :: undf_aspc1
    integer(kind=i_def), intent(in), dimension(ndf_aspc1) :: map_aspc1
    real(kind=r_def), intent(inout), dimension(undf_aspc1) :: field_1_aspc1
    real(kind=r_def), intent(in),    dimension(undf_aspc1) :: field_2_aspc1
    real(kind=r_def), intent(in),    dimension(undf_aspc1) :: field_3_aspc1

    ! Internal variables
    integer(kind=i_def) :: k, df

    ! Update field
    do k = 0, nlayers-1
      do df = 1, ndf_aspc1
        field_1_aspc1( map_aspc1(df) + k ) = &
          field_2_aspc1( map_aspc1(df) + k ) + field_3_aspc1( map_aspc1(df) + k )
      end do
    end do

  end subroutine add_fields_any_code

end module add_fields_any_kernel_mod
