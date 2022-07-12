module compute_unew_mod
  USE kind_params_mod
  USE kernel_mod
  use argument_mod
  use grid_mod
  use field_mod
  implicit none

  private

  public compute_unew, compute_unew_code

  type, extends(kernel_type) :: compute_unew
     type(go_arg), dimension(7) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),        & ! unew
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        & ! uold
             go_arg(GO_READ,  GO_CF, GO_STENCIL(010,010,000)),        & ! z
             go_arg(GO_READ,  GO_CV, GO_STENCIL(110,110,000)),        & ! cv
             go_arg(GO_READ,  GO_CT, GO_STENCIL(000,110,000)),        & ! h
             go_arg(GO_READ,  GO_R_SCALAR, GO_POINTWISE),  & ! tdt
             go_arg(GO_READ,  GO_GRID_DY_CONST)            & ! dy
           /)
     !> This kernel operates on fields that live on an
     !! orthogonal, regular grid.
     integer :: GRID_TYPE = GO_ORTHOGONAL_REGULAR

     !> We only have one value per grid point and that means
     !! we have a single DOF per grid point.
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
    procedure, nopass :: code => compute_unew_code
  end type compute_unew

contains

  !===================================================

  subroutine compute_unew_code(i, j,  &
                               unew, uold, z, cv, h, tdt, dx)
    implicit none
    integer,  intent(in) :: I, J
    real(go_wp), intent(in) :: dx
    real(go_wp), intent(out), dimension(:,:) :: unew
    real(go_wp), intent(in),  dimension(:,:) :: uold, z, cv, h
    real(go_wp), intent(in) :: tdt
    ! Locals
    real(go_wp) :: tdts8, tdtsdx

    !> These quantities are computed here because tdt is not
    !! constant. (It is == dt for first time step, 2xdt for
    !! all remaining time steps.)
    tdts8 = tdt/8.0d0
    tdtsdx = tdt/dx

    UNEW(I,J) = UOLD(I,J) +                     &
                TDTS8*(Z(I,J+1) + Z(I,J)) * &
                (CV(I,J+1)+CV(I-1,J+1)+CV(I-1,J)+CV(I,J)) -   &
                TDTSDX*(H(I,J)-H(I-1,J))

  end subroutine compute_unew_code

end module compute_unew_mod
