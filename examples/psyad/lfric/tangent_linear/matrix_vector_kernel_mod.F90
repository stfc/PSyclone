!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------

!> @brief This version is for use on continuous spaces and will force
!>        halo exchanges on the input fields
module matrix_vector_kernel_mod
  use argument_mod,            only : arg_type,                 &
                                      GH_FIELD, GH_OPERATOR,    &
                                      GH_REAL, GH_READ, GH_INC, &
                                      ANY_SPACE_1, ANY_SPACE_2, &
                                      CELL_COLUMN
  use constants_mod,           only : i_def, r_single, r_double
  use kernel_mod,              only : kernel_type

  implicit none

  private

  !-------------------------------------------------------------------------------
  ! Public types
  !-------------------------------------------------------------------------------
  type, public, extends(kernel_type) :: matrix_vector_kernel_type
    private
    type(arg_type) :: meta_args(3) = (/                                    &
         arg_type(GH_FIELD,    GH_REAL, GH_INC,  ANY_SPACE_1),             &
         arg_type(GH_FIELD,    GH_REAL, GH_READ, ANY_SPACE_2),             &
         arg_type(GH_OPERATOR, GH_REAL, GH_READ, ANY_SPACE_1, ANY_SPACE_2) &
         /)
    integer :: operates_on = CELL_COLUMN
  end type

  !-------------------------------------------------------------------------------
  ! Contained functions/subroutines
  !-------------------------------------------------------------------------------
  public :: matrix_vector_code

  ! Generic interface for real32 and real64 types
  interface matrix_vector_code
    module procedure  &
      matrix_vector_code_r_single, &
      matrix_vector_code_r_double
  end interface

contains

  !> @brief Computes lhs = matrix*x
  !> @brief real32 and real64 variants
  !! @param[in] cell Horizontal cell index
  !! @param[in] nlayers Number of layers
  !! @param[inout] lhs Output lhs (A*x)
  !! @param[in] x Input data
  !! @param[in] ncell_3d Total number of cells
  !! @param[in] matrix Local matrix assembly form of the operator A
  !! @param[in] ndf1 Number of degrees of freedom per cell for the output field
  !! @param[in] undf1 Unique number of degrees of freedom  for the output field
  !! @param[in] map1 Dofmap for the cell at the base of the column for the
  !! output field
  !! @param[in] ndf2 Number of degrees of freedom per cell for the input field
  !! @param[in] undf2 Unique number of degrees of freedom for the input field
  !! @param[in] map2 Dofmap for the cell at the base of the column for the input
  !! field

  ! R_SINGLE PRECISION
  ! ==================
  subroutine matrix_vector_code_r_single(cell,              &
                                         nlayers,           &
                                         lhs, x,            &
                                         ncell_3d,          &
                                         matrix,            &
                                         ndf1, undf1, map1, &
                                         ndf2, undf2, map2)

    implicit none

    ! Arguments
    integer(kind=i_def),                  intent(in) :: cell, nlayers, ncell_3d
    integer(kind=i_def),                  intent(in) :: undf1, ndf1
    integer(kind=i_def),                  intent(in) :: undf2, ndf2
    integer(kind=i_def), dimension(ndf1), intent(in) :: map1
    integer(kind=i_def), dimension(ndf2), intent(in) :: map2
    real(kind=r_single), dimension(undf2),              intent(in)    :: x
    real(kind=r_single), dimension(undf1),              intent(inout) :: lhs
    real(kind=r_single), dimension(ndf1,ndf2,ncell_3d), intent(in)    :: matrix

    ! Internal variables
    integer(kind=i_def)                  :: df, k, ik
    real(kind=r_single), dimension(ndf2) :: x_e
    real(kind=r_single), dimension(ndf1) :: lhs_e

    do k = 0, nlayers-1
      do df = 1, ndf2
        x_e(df) = x(map2(df)+k)
      end do
      ik = (cell-1)*nlayers + k + 1
      lhs_e = matmul(matrix(:,:,ik),x_e)
      do df = 1,ndf1
        lhs(map1(df)+k) = lhs(map1(df)+k) + lhs_e(df)
      end do
    end do

  end subroutine matrix_vector_code_r_single


  ! R_DOUBLE PRECISION
  ! ==================
  subroutine matrix_vector_code_r_double(cell,              &
                                         nlayers,           &
                                         lhs, x,            &
                                         ncell_3d,          &
                                         matrix,            &
                                         ndf1, undf1, map1, &
                                         ndf2, undf2, map2)

    implicit none

    ! Arguments
    integer(kind=i_def),                  intent(in) :: cell, nlayers, ncell_3d
    integer(kind=i_def),                  intent(in) :: undf1, ndf1
    integer(kind=i_def),                  intent(in) :: undf2, ndf2
    integer(kind=i_def), dimension(ndf1), intent(in) :: map1
    integer(kind=i_def), dimension(ndf2), intent(in) :: map2
    real(kind=r_double), dimension(undf2),              intent(in)    :: x
    real(kind=r_double), dimension(undf1),              intent(inout) :: lhs
    real(kind=r_double), dimension(ndf1,ndf2,ncell_3d), intent(in)    :: matrix

    ! Internal variables
    integer(kind=i_def)                  :: df, k, ik
    real(kind=r_double), dimension(ndf2) :: x_e
    real(kind=r_double), dimension(ndf1) :: lhs_e

    do k = 0, nlayers-1
      do df = 1, ndf2
        x_e(df) = x(map2(df)+k)
      end do
      ik = (cell-1)*nlayers + k + 1
      lhs_e = matmul(matrix(:,:,ik),x_e)
      do df = 1,ndf1
        lhs(map1(df)+k) = lhs(map1(df)+k) + lhs_e(df)
      end do
    end do

  end subroutine matrix_vector_code_r_double

end module matrix_vector_kernel_mod
