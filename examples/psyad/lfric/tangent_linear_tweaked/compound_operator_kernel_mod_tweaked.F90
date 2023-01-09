!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!> @brief Computes a compound operator A = B*C*D where A,B,C & D are all locally
!!        assembled operators and B & C are mass matrices
module compound_operator_kernel_mod

use constants_mod,           only: r_def, i_def
use kernel_mod,              only: kernel_type
use argument_mod,            only: arg_type,                 &
                                   GH_OPERATOR, GH_FIELD,    &
                                   GH_SCALAR, GH_REAL,       &
                                   GH_READ, GH_WRITE,        &
                                   ANY_SPACE_1, ANY_SPACE_2, &
                                   CELL_COLUMN

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: compound_operator_kernel_type
  private
  type(arg_type) :: meta_args(6) = (/                                      &
       arg_type(GH_OPERATOR, GH_REAL, GH_WRITE, ANY_SPACE_1, ANY_SPACE_2), &
       arg_type(GH_OPERATOR, GH_REAL, GH_READ,  ANY_SPACE_1, ANY_SPACE_1), &
       arg_type(GH_OPERATOR, GH_REAL, GH_READ,  ANY_SPACE_1, ANY_SPACE_1), &
       arg_type(GH_OPERATOR, GH_REAL, GH_READ,  ANY_SPACE_1, ANY_SPACE_2), &
       arg_type(GH_FIELD,    GH_REAL, GH_READ,  ANY_SPACE_2),              &
       arg_type(GH_SCALAR,   GH_REAL, GH_READ)                             &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: compound_operator_kernel_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: compound_operator_kernel_code
contains

!> @brief This subroutine computes the div operator
!! @param[in] cell Cell number
!! @param[in] nlayers Number of layers.
!! @param[in] ncell_3d_1 Ncell*ndf
!! @param[in] ncell_3d_2 Ncell*ndf
!! @param[in] ncell_3d_3 Ncell*ndf
!! @param[in] ncell_3d_4 Ncell*ndf
!! @param[in,out] compound_operator LMA operator to create
!! @param[in] mass_matrix1 First mass matrix
!! @param[in] mass_matrix2 Second mass matrix
!! @param[in] differential_matrix Third operator
!! @param[in] field weighting field
!! @param[in] tau scalar weighting
!! @param[in] ndf1 Number of dofs per cell for space 1
!! @param[in] ndf2 Number of dofs per cell for space 2
!! @param[in] undf2 total Number of dofs per cell for space 2
!! @param[in] map2 dofmap for space 2

subroutine compound_operator_kernel_code(cell, nlayers, &
                                         ncell_3d_1,  &
                                         compound_operator, &
                                         ncell_3d_2,  &
                                         mass_matrix1,  &
                                         ncell_3d_3,  &
                                         mass_matrix2, &
                                         ncell_3d_4,  &
                                         differential_matrix, &
                                         field, &
                                         tau, &
                                         ndf1, ndf2, undf2, map2)
  implicit none

  ! Arguments
  integer(kind=i_def),                     intent(in) :: cell
  integer(kind=i_def),                     intent(in) :: nlayers
  integer(kind=i_def),                     intent(in) :: ncell_3d_1, ncell_3d_2, ncell_3d_3, ncell_3d_4
  integer(kind=i_def),                     intent(in) :: ndf1, ndf2, undf2
  integer(kind=i_def), dimension(ndf2),    intent(in) :: map2


  real(kind=r_def), dimension(ndf1,ndf2,ncell_3d_1), intent(inout) :: compound_operator
  real(kind=r_def), dimension(ndf1,ndf1,ncell_3d_2), intent(in)    :: mass_matrix1
  real(kind=r_def), dimension(ndf1,ndf1,ncell_3d_3), intent(in)    :: mass_matrix2
  real(kind=r_def), dimension(ndf1,ndf2,ncell_3d_4), intent(in)    :: differential_matrix
  real(kind=r_def), dimension(undf2),                intent(in)    :: field
  real(kind=r_def),                                  intent(in)    :: tau

  real(kind=r_def), dimension(ndf1,ndf2) :: tmp
 
  ! Internal variables
  integer(kind=i_def)                     :: k, ik, df
  real(kind=r_def),  dimension(ndf1,ndf2) :: d

  do k = 0, nlayers - 1
    ik = k + 1 + (cell-1)*nlayers
    tmp = matmul(mass_matrix2(:,:,ik), differential_matrix(:,:,ik))
    d = matmul(mass_matrix1(:,:,ik), tmp)
    do df = 1,ndf2
      compound_operator(:,df,ik) = tau*d(:,df)*field(map2(df)+k)
    end do
  end do

end subroutine compound_operator_kernel_code

end module compound_operator_kernel_mod
