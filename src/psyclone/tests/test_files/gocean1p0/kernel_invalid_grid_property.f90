module kernel_invalid_grid_property
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
          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),        & ! cu
             go_arg(GO_READ,  GO_CT, GO_POINTWISE),        & ! p
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        & ! u
             go_arg(GO_READ,  GRID_AREA_WRONG)       &
           /)
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the South and West of it.
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

end module kernel_invalid_grid_property
