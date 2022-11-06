!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Transpose a matrix in local operator representation
!> @details Given a matrix \f$A\f$ calculate the transpose \f$B=A^T\f$

module transpose_matrix_kernel_mod

use argument_mod,            only : arg_type,                 &
                                    GH_OPERATOR, GH_REAL,     &
                                    GH_READ, GH_WRITE,        &
                                    ANY_SPACE_1, ANY_SPACE_2, &
                                    CELL_COLUMN
use constants_mod,           only : r_single, r_double, i_def
use kernel_mod,              only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: transpose_matrix_kernel_type
  private
  type(arg_type) :: meta_args(2) = (/                                      &
       arg_type(GH_OPERATOR, GH_REAL, GH_READ,  ANY_SPACE_1, ANY_SPACE_2), &
       arg_type(GH_OPERATOR, GH_REAL, GH_WRITE, ANY_SPACE_2, ANY_SPACE_1)  &
       /)
  integer :: operates_on = CELL_COLUMN
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: transpose_matrix_code

  ! Generic interface for real32 and real64 types
  interface transpose_matrix_code
    module procedure  &
      transpose_matrix_code_r_single, &
      transpose_matrix_code_r_double
  end interface

contains

!> @brief Computes the transpose of a matrix
!> @param[in]  cell Horizontal cell index
!> @param[in]  nlayers Number of layers
!> @param[in]  ncell_3d Total number of cells
!> @param[in]  mat_in Input matrix
!> @param[in]  ncell_3d_2 Total number of cells (passed in twice)
!> @param[in,out] mat_out Resulting transposed matrix
!> @param[in]  ndf1 Number of degrees of freedom per cell for space 1
!> @param[in]  ndf2 Number of degrees of freedom per cell for space 2

! R_SINGLE PRECISION
! ==================
subroutine transpose_matrix_code_r_single(cell,        &
                                          nlayers,     &
                                          ncell_3d,    &
                                          mat_in,      &
                                          ncell_3d_2,  &
                                          mat_out,     &
                                          ndf1,        &
                                          ndf2)

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in)    :: cell
  integer(kind=i_def), intent(in)    :: nlayers
  integer(kind=i_def), intent(in)    :: ncell_3d
  integer(kind=i_def), intent(in)    :: ncell_3d_2
  integer(kind=i_def), intent(in)    :: ndf1
  integer(kind=i_def), intent(in)    :: ndf2
  real(kind=r_single), dimension(ndf1,ndf2,ncell_3d), intent(in)      :: mat_in
  real(kind=r_single), dimension(ndf2,ndf1,ncell_3d), intent(inout)   :: mat_out

  ! Internal variables
  integer(kind=i_def) :: k, ik

  do k = 0, nlayers-1
    ik = (cell-1)*nlayers + k + 1
    mat_out(:,:,ik) = transpose(mat_in(:,:,ik))
  end do

end subroutine transpose_matrix_code_r_single

! R_DOUBLE PRECISION
! ==================
subroutine transpose_matrix_code_r_double(cell,        &
                                          nlayers,     &
                                          ncell_3d,    &
                                          mat_in,      &
                                          ncell_3d_2,  &
                                          mat_out,     &
                                          ndf1,        &
                                          ndf2)

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in)    :: cell
  integer(kind=i_def), intent(in)    :: nlayers
  integer(kind=i_def), intent(in)    :: ncell_3d
  integer(kind=i_def), intent(in)    :: ncell_3d_2
  integer(kind=i_def), intent(in)    :: ndf1
  integer(kind=i_def), intent(in)    :: ndf2
  real(kind=r_double), dimension(ndf1,ndf2,ncell_3d), intent(in)      :: mat_in
  real(kind=r_double), dimension(ndf2,ndf1,ncell_3d), intent(inout)   :: mat_out

  ! Internal variables
  integer(kind=i_def) :: k, ik

  do k = 0, nlayers-1
    ik = (cell-1)*nlayers + k + 1
    mat_out(:,:,ik) = transpose(mat_in(:,:,ik))
  end do

end subroutine transpose_matrix_code_r_double

end module transpose_matrix_kernel_mod
