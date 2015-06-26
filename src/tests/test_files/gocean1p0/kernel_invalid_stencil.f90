module kernel_invalid_stencil
  !use kind_params_mod
  !use kernel_mod
  !use argument_mod
  !use field_mod
  !use grid_mod
  implicit none

  private

  public compute_cu, compute_cu_code

  type, extends(kernel_type) :: compute_cu
     type(arg), dimension(4) :: meta_args =    &
          ! We deliberately specify an incorrect stencil value
          ! for the first kernel argument in order to test the 
          ! parser...
          (/ arg(WRITE, CU, POINTS),        & ! cu
             arg(READ,  CT, POINTWISE),        & ! p
             arg(READ,  CU, POINTWISE),        & ! u
             arg(READ,  GRID_AREA_T)       &
           /)
     integer :: ITERATES_OVER = INTERNAL_PTS

     integer :: index_offset = OFFSET_SW

  contains
    procedure, nopass :: code => compute_cu_code
  end type compute_cu

contains

  !===================================================

  !> Compute the mass flux in the x direction at point (i,j)
  subroutine compute_cu_code(i, j, cu, p, u)
    implicit none
    integer,  intent(in) :: I, J
    real(wp), intent(out), dimension(:,:) :: cu
    real(wp), intent(in),  dimension(:,:) :: p, u

    CU(I,J) = 0.5d0*(P(i+1,J)+P(I,J))*U(I,J)

  end subroutine compute_cu_code

end module kernel_invalid_stencil
