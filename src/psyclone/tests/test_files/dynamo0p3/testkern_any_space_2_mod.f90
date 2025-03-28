! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2025, Science and Technology Facilities Council
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

module testkern_any_space_2_mod

  use argument_mod
  use kernel_mod
  use constants_mod, only : r_def, i_def

  implicit none

  ! Test for any_space producing correct code where there are
  ! 1) multi declarations of the same any_space space,
  ! 2) no other spaces in the arguments,
  ! 3) no functions (e.g. basis, diff_basis) declared,
  ! 4) any_space used with an operator.
  type, public, extends(kernel_type) ::testkern_any_space_2_type
    private
    type(arg_type) :: meta_args(4) = (/                           &
         arg_type(GH_FIELD,    GH_REAL,    GH_INC,  ANY_SPACE_1), &
         arg_type(GH_FIELD,    GH_REAL,    GH_READ, ANY_SPACE_1), &
         arg_type(GH_OPERATOR, GH_REAL,    GH_READ, ANY_SPACE_1,  &
                                                    ANY_SPACE_1), &
         arg_type(GH_SCALAR,   GH_INTEGER, GH_READ)               &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, public, nopass :: testkern_any_space_2_code
  end type testkern_any_space_2_type

contains

  subroutine testkern_any_space_2_code(cell, nlayers, f1_data, f2_data, &
                                       ncell_3d, local_stencil, istep,  &
                                       ndf_aspc1, undf_aspc1, map_aspc1)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_aspc1
    integer(kind=i_def), intent(in) :: undf_aspc1
    integer(kind=i_def), intent(in) :: istep
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in), dimension(ndf_aspc1) :: map_aspc1
    real(kind=r_def), intent(inout), dimension(undf_aspc1) :: f1_data
    real(kind=r_def), intent(in), dimension(undf_aspc1)    :: f2_data
    real(kind=r_def), intent(in), dimension(ncell_3d,ndf_aspc1,ndf_aspc1) :: local_stencil

  end subroutine testkern_any_space_2_code

end module testkern_any_space_2_mod
