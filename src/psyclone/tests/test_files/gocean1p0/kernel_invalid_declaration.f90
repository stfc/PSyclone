module kernel_invalid_fortran
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use field_mod
  use grid_mod
  implicit none

  private

  public compute, compute_code

  type, extends(kernel_type) :: compute
     type(go_arg), dimension(3) :: meta_args =       &
          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),  & ! cu
             go_arg(GO_READ,  GO_CT, GO_POINTWISE),  & ! p
             go_arg(GO_READ,  GO_CU, GO_POINTWISE)   & ! u
           /)
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_code
  end type compute

contains

  !===================================================

  ! Forget to declare u
  subroutine compute_code(i, j, cu, p, u)
    implicit none
    integer,  intent(in) :: I, J
    real(go_wp), intent(out), dimension(:,:) :: cu
    real(go_wp), intent(in),  dimension(:,:) :: p

    CU(I,J) = 0.5d0*(P(i+1,J)+P(I,J))*U(I,J)

  end subroutine compute_code

end module kernel_invalid_fortran
