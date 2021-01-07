!-----------------------------------------------------------------------------
! (C) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2021, Science and Technology Facilities Council
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
!------------------------------------------------------------------------------
! Modified by I. Kavcic, Met Office
!------------------------------------------------------------------------------

module integer_double_kernel_mod

use argument_mod,            only : arg_type,                    &
                                    GH_FIELD, GH_WRITE, GH_READ, &
                                    GH_INTEGER, GH_REAL,         &
                                    ANY_DISCONTINUOUS_SPACE_1,   &
                                    CELLS
use constants_mod,           only : r_def, i_def
use kernel_mod,              only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: integer_double_kernel_type
  private
  type(arg_type) :: meta_args(2) = (/                                       &
       arg_type(GH_FIELD, GH_INTEGER, GH_WRITE, ANY_DISCONTINUOUS_SPACE_1), &
       arg_type(GH_FIELD, GH_REAL,    GH_READ,  ANY_DISCONTINUOUS_SPACE_1)  &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: integer_double_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public integer_double_code

contains

!> @brief Takes a real field, doubles it and returns an integer field of the
!>        truncated real values
!! @param[in] nlayers Number of layers
!! @param[in,out] integer_field Field to be returned
!! @param[in] real_field Field to be doubled
!! @param[in] ndf1 Number of degrees of freedom per cell for the output field
!! @param[in] undf1 Unique number of degrees of freedom  for the output field
!! @param[in] map1 Dofmap for the cell at the base of the column for the output field
subroutine integer_double_code(nlayers,          &
                               integer_field,    &
                               real_field,       &
                               ndf1, undf1, map1)

  implicit none

  ! Arguments
  integer(kind=i_def),                   intent(in)    :: nlayers
  integer(kind=i_def),                   intent(in)    :: undf1, ndf1
  integer(kind=i_def), dimension(ndf1),  intent(in)    :: map1
  integer(kind=i_def), dimension(undf1), intent(inout) :: integer_field
  real(kind=r_def),    dimension(undf1), intent(in)    :: real_field

  ! Internal variables
  integer(kind=i_def)               :: df, k

  do k = 0, nlayers-1
    do df = 1, ndf1
      integer_field(map1(df)+k) = int ( real_field(map1(df)+k) * 2.0_r_def )
    end do
  end do

end subroutine integer_double_code

end module integer_double_kernel_mod
