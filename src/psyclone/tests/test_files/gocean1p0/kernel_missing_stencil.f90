module kernel_missing_stencil
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use field_mod
  use grid_mod
  implicit none

  private

  public compute_cu, compute_cu_code

  type, extends(kernel_type) :: compute_cu
     type(go_arg), dimension(4) :: meta_args =    &
          ! We deliberately miss-out the 'GO_POINTWISE' stencil value
          ! for the first kernel argument in order to test the 
          ! parser...
          (/ go_arg(GO_WRITE, GO_CU),        & ! cu
             go_arg(GO_READ,  GO_CT, GO_POINTWISE),    & ! p
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),    & ! u
             go_arg(GO_READ,  GO_GRID_AREA_T)          &
           /)
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_cu_code
  end type compute_cu

contains

  !===================================================

  !> Compute the mass flux in the x direction at point (i,j)
  subroutine compute_cu_code(i, j, cu, p, u)
    implicit none
    integer,  intent(in) :: I, J
    real(go_wp), intent(out), dimension(:,:) :: cu
    real(go_wp), intent(in),  dimension(:,:) :: p, u

    CU(I,J) = 0.5d0*(P(i+1,J)+P(I,J))*U(I,J)

  end subroutine compute_cu_code

end module kernel_missing_stencil
