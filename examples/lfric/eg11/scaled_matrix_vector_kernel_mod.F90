!-----------------------------------------------------------------------------
! (C) Crown copyright 2018 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
! -----------------------------------------------------------------------------
!
! BSD 3-Clause License
!
! Modifications copyright (c) 2018-2020, Science and Technology Facilities Council
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
! Modified I. Kavcic, Met Office

module scaled_matrix_vector_kernel_mod

  use argument_mod,      only : arg_type,              &
                                GH_FIELD, GH_OPERATOR, &
                                GH_READ, GH_INC,       &
                                CELLS
  use constants_mod,     only : r_def, i_def
  use fs_continuity_mod, only : W2, W3
  use kernel_mod,        only : kernel_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------

  type, public, extends(kernel_type) :: scaled_matrix_vector_kernel_type
    private
    type(arg_type) :: meta_args(5) = (/         &
        arg_type(GH_FIELD,    GH_INC,  W2),     &
        arg_type(GH_FIELD,    GH_READ, W3),     &
        arg_type(GH_OPERATOR, GH_READ, W2, W3), &
        arg_type(GH_FIELD,    GH_READ, W2),     &
        arg_type(GH_FIELD,    GH_READ, W2)      &
        /)
    integer :: iterates_over = CELLS
  contains
    procedure, nopass :: scaled_matrix_vector_code
  end type

  type, public, extends(kernel_type) :: opt_scaled_matrix_vector_kernel_type
    private
    type(arg_type) :: meta_args(5) = (/         &
        arg_type(GH_FIELD,    GH_INC,  W2),     &
        arg_type(GH_FIELD,    GH_READ, W3),     &
        arg_type(GH_OPERATOR, GH_READ, W2, W3), &
        arg_type(GH_FIELD,    GH_READ, W2),     &
        arg_type(GH_FIELD,    GH_READ, W2)      &
          /)
    integer :: iterates_over = CELLS
  contains
    procedure, nopass :: opt_scaled_matrix_vector_code
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public scaled_matrix_vector_code
  public opt_scaled_matrix_vector_code

contains

!> @brief Computes lhs = y*matrix*x where matrix maps from x space to lhs space
!>        and y is a field in the same space as lhs
!> @param[in] cell Horizontal cell index
!! @param[in] nlayers Number of layers
!> @param[inout] lhs Output lhs (A*x)
!! @param[in] x Input data
!! @param[in] ncell_3d Total number of cells
!! @param[in] matrix Local matrix assembly form of the operator A
!! @param[in] y Field to scale output by
!! @param[in] z Second field to scale output by
!! @param[in] ndf1 Number of degrees of freedom per cell for the output field
!! @param[in] undf1 Unique number of degrees of freedom  for the output field
!! @param[in] map1 Dofmap for the cell at the base of the column for the output field
!! @param[in] ndf2 Number of degrees of freedom per cell for the input field
!! @param[in] undf2 Unique number of degrees of freedom for the input field
!! @param[in] map2 Dofmap for the cell at the base of the column for the input field
subroutine scaled_matrix_vector_code(cell,              &
                                     nlayers,           &
                                     lhs, x,            &
                                     ncell_3d,          &
                                     matrix,            &
                                     y,                 &
                                     z,                 &
                                     ndf1, undf1, map1, &
                                     ndf2, undf2, map2)

  implicit none

  ! Arguments
  integer(kind=i_def),                   intent(in) :: cell, nlayers, ncell_3d
  integer(kind=i_def),                   intent(in) :: undf1, ndf1
  integer(kind=i_def),                   intent(in) :: undf2, ndf2
  integer(kind=i_def), dimension(ndf1),  intent(in) :: map1
  integer(kind=i_def), dimension(ndf2),  intent(in) :: map2

  real(kind=r_def), dimension(undf2),              intent(in)    :: x
  real(kind=r_def), dimension(undf1),              intent(inout) :: lhs
  real(kind=r_def), dimension(ndf1,ndf2,ncell_3d), intent(in)    :: matrix
  real(kind=r_def), dimension(undf1),              intent(in)    :: y
  real(kind=r_def), dimension(undf1),              intent(in)    :: z

  ! Internal variables
  integer(kind=i_def)               :: df, k, ik
  real(kind=r_def), dimension(ndf2) :: x_e
  real(kind=r_def), dimension(ndf1) :: lhs_e

  do k = 0, nlayers-1
    do df = 1, ndf2
      x_e(df) = x(map2(df)+k)
    end do
    ik = (cell-1)*nlayers + k + 1
    lhs_e = matmul(matrix(:,:,ik),x_e)
    do df = 1,ndf1
       lhs(map1(df)+k) = lhs(map1(df)+k) + lhs_e(df)*y(map1(df)+k)*z(map1(df)+k)
    end do
  end do

end subroutine scaled_matrix_vector_code

!=============================================================================!
!> @brief Computes lhs = y*matrix*x where matrix maps from x space to lhs space
!>        and y is a field in the same space as lhs which has been optimised for
!>        lhs, y in W2, x in W3 all at lowest order
!> @param[in] cell Horizontal cell index
!! @param[in] nlayers Number of layers
!> @param[inout] lhs Output lhs (A*x)
!! @param[in] x Input data
!! @param[in] ncell_3d Total number of cells
!! @param[in] matrix Local matrix assembly form of the operator A
!! @param[in] y Field to scale output by
!! @param[in] z Second field to scale output by
!! @param[in] ndf1 Number of degrees of freedom per cell for the output field
!! @param[in] undf1 Unique number of degrees of freedom  for the output field
!! @param[in] map1 Dofmap for the cell at the base of the column for the output field
!! @param[in] ndf2 Number of degrees of freedom per cell for the input field
!! @param[in] undf2 Unique number of degrees of freedom for the input field
!! @param[in] map2 Dofmap for the cell at the base of the column for the input field
subroutine opt_scaled_matrix_vector_code(cell,              &
                                         nlayers,           &
                                         lhs, x,            &
                                         ncell_3d,          &
                                         matrix,            &
                                         y,                 &
                                         z,                 &
                                         ndf1, undf1, map1, &
                                         ndf2, undf2, map2)

  implicit none

  ! Arguments
  integer(kind=i_def),                   intent(in) :: cell, nlayers, ncell_3d
  integer(kind=i_def),                   intent(in) :: undf1, ndf1
  integer(kind=i_def),                   intent(in) :: undf2, ndf2
  integer(kind=i_def), dimension(ndf1),  intent(in) :: map1
  integer(kind=i_def), dimension(ndf2),  intent(in) :: map2
  real(kind=r_def), dimension(undf2),        intent(in)    :: x
  real(kind=r_def), dimension(undf1),        intent(inout) :: lhs
  real(kind=r_def), dimension(6,1,ncell_3d), intent(in)    :: matrix
  real(kind=r_def), dimension(undf1),        intent(in)    :: y
  real(kind=r_def), dimension(undf1),        intent(in)    :: z

  ! Internal variables
  integer(kind=i_def) :: k, ik, df

  ! Hard wired optimisation for desired configuration
  do df = 1,6
    do k = 0, nlayers-1
      ik = (cell-1)*nlayers + k + 1
      lhs(map1(df)+k) = lhs(map1(df)+k) + matrix(df,1,ik)*x(map2(1)+k)*y(map1(df)+k)*z(map1(df)+k)
    end do
  end do

  ! Apply zero flux boundary conditions
  lhs(map1(5))             = 0.0_r_def
  lhs(map1(6) + nlayers-1) = 0.0_r_def

end subroutine opt_scaled_matrix_vector_code

end module scaled_matrix_vector_kernel_mod
