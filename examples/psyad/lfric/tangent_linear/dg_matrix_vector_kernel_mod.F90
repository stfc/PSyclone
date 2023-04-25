!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief This version is for use on discontinous spaces and over-writes the
!>        output field entirely, hence can only be used for W3 spaces
module dg_matrix_vector_kernel_mod

  use constants_mod, only : r_def, i_def
  use kernel_mod,    only : kernel_type
  use argument_mod,  only : arg_type,                  &
                            GH_FIELD, GH_OPERATOR,     &
                            GH_READ, GH_WRITE,         &
                            GH_REAL, ANY_SPACE_1,      &
                            ANY_DISCONTINUOUS_SPACE_1, &
                            CELL_COLUMN

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------

  type, public, extends(kernel_type) :: dg_matrix_vector_kernel_type
    private
    type(arg_type) :: meta_args(3) = (/                                       &
         arg_type(GH_FIELD,    GH_REAL, GH_WRITE, ANY_DISCONTINUOUS_SPACE_1), &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  ANY_SPACE_1),               &
         arg_type(GH_OPERATOR, GH_REAL, GH_READ,  ANY_DISCONTINUOUS_SPACE_1,  &
                                                  ANY_SPACE_1)                &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: dg_matrix_vector_code
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public :: dg_matrix_vector_code

contains

!> @brief Computes lhs = matrix*x for discontinuous function spaces
!> @param[in] cell Horizontal cell index
!> @param[in] nlayers Number of layers
!> @param[in,out] lhs Output lhs (A*x)
!> @param[in] x input data
!> @param[in] ncell_3d Total number of cells
!> @param[in] matrix Matrix values in LMA form
!> @param[in] ndf1 Number of degrees of freedom per cell for the output field
!> @param[in] undf1 Unique number of degrees of freedom  for the output field
!> @param[in] map1 Dofmap for the cell at the base of the column for the output field
!> @param[in] ndf2 Number of degrees of freedom per cell for the input field
!> @param[in] undf2 Unique number of degrees of freedom for the input field
!> @param[in] map2 Dofmap for the cell at the base of the column for the input field
subroutine dg_matrix_vector_code(cell,              &
                                 nlayers,           &
                                 lhs, x,            &
                                 ncell_3d,          &
                                 matrix,            &
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
       lhs(map1(df)+k) = lhs_e(df)
    end do
  end do

end subroutine dg_matrix_vector_code

end module dg_matrix_vector_kernel_mod
