! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2019-2024, Science and Technology Facilities Council
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
! Author I. Kavcic, Met Office

module testkern_any_discontinuous_space_op_1_mod

  use constants_mod
  use argument_mod
  use kernel_mod

  implicit none

  ! Description: test for any_discontinuous_space producing correct code
  ! when there are
  ! 1) multiple declarations of the same any_discontinuous_space,
  ! 2) other any_discontinuous_space spaces in the arguments,
  ! 3) no functions (e.g. basis, diff_basis) declared,
  ! 4) any_discontinuous_space used with an operator,
  ! 5) different to- and from- any_discontinuous_space spaces used with
  !    an operator.

  type, public, extends(kernel_type) :: testkern_any_discontinuous_space_op_1_type
    private
    type(arg_type) :: meta_args(5) = (/                                           &
         arg_type(GH_FIELD*3,  GH_REAL, GH_READ,      ANY_DISCONTINUOUS_SPACE_1), &
         arg_type(GH_FIELD,    GH_REAL, GH_READWRITE, ANY_DISCONTINUOUS_SPACE_2), &
         arg_type(GH_OPERATOR, GH_REAL, GH_READ,      ANY_DISCONTINUOUS_SPACE_1,  &
                                                      ANY_DISCONTINUOUS_SPACE_1), &
         arg_type(GH_OPERATOR, GH_REAL, GH_WRITE,     ANY_DISCONTINUOUS_SPACE_3,  &
                                                      ANY_DISCONTINUOUS_SPACE_7), &
         arg_type(GH_SCALAR,   GH_REAL, GH_READ)                                  &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, public, nopass :: testkern_any_discontinuous_space_op_1_code
  end type testkern_any_discontinuous_space_op_1_type

contains

  subroutine testkern_any_discontinuous_space_op_1_code(              &
                                        cell, nlayers,                &
                                        field1_x, field1_y, field1_z, &
                                        field2,                       &
                                        ncell_3d_op3, op3,            &
                                        ncell_3d_op4, op4,            &
                                        rscalar,                      &
                                        ndf1, undf1, map1,            &
                                        ndf2, undf2, map2,            &
                                        ndf_to_op4, ndf_from_op4)

      implicit none

      integer(kind=i_def), intent(in) :: nlayers
      integer(kind=i_def), intent(in) :: ndf1, ndf2
      integer(kind=i_def), intent(in) :: undf1, undf2
      integer(kind=i_def), intent(in) :: ndf_to_op4, ndf_from_op4
      integer(kind=i_def), intent(in) :: cell
      integer(kind=i_def), intent(in) :: ncell_3d_op3
      integer(kind=i_def), intent(in) :: ncell_3d_op4
      integer(kind=i_def), intent(in), dimension(ndf1) :: map1
      integer(kind=i_def), intent(in), dimension(ndf2) :: map2
      real(kind=r_def), intent(in) :: rscalar
      real(kind=r_def), intent(in), dimension(undf1)    :: field1_x, field1_y, &
                                                           field1_z
      real(kind=r_def), intent(inout), dimension(undf2) :: field2
      real(kind=r_def), intent(in), dimension(ndf1,ndf1,ncell_3d_op3)                  :: op3
      real(kind=r_def), intent(inout), dimension(ndf_to_op4,ndf_from_op4,ncell_3d_op4) :: op4

  end subroutine testkern_any_discontinuous_space_op_1_code

end module testkern_any_discontinuous_space_op_1_mod
