module compute_vnew_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod
  use field_mod
  implicit none

  private

  public compute_vnew, compute_vnew_code

  TYPE, EXTENDS(kernel_type) :: compute_vnew
     TYPE(go_arg), DIMENSION(7) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CV, GO_POINTWISE),        & ! vnew
             go_arg(GO_READ,  GO_CV, GO_POINTWISE),        & ! vold
             go_arg(GO_READ,  GO_CF, GO_STENCIL(000,011,000)),        & ! z
             go_arg(GO_READ,  GO_CU, GO_STENCIL(000,011,011)),        & ! cu
             go_arg(GO_READ,  GO_CT, GO_STENCIL(000,010,010)),        & ! h
             go_arg(GO_READ,  GO_R_SCALAR, GO_POINTWISE),  & ! tdt
             go_arg(GO_READ,  GO_GRID_DY_CONST)         & ! dy
           /)
     !> This kernel operates on fields that live on an
     !! orthogonal, regular grid.
     integer :: GRID_TYPE = GO_ORTHOGONAL_REGULAR

     !> We only have one value per grid point and that means
     !! we have a single DOF per grid point.
     INTEGER :: ITERATES_OVER = GO_INTERNAL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the South and West of it.
     integer :: index_offset = GO_OFFSET_SW

  CONTAINS
    procedure, nopass :: code => compute_vnew_code
  END TYPE compute_vnew

CONTAINS

  !===================================================

  subroutine compute_vnew_code(i, j, &
                               vnew, vold, z, cu, h, tdt, dy)
    implicit none
    integer,  intent(in) :: I, J
    real(go_wp), intent(in) :: dy
    REAL(go_wp), intent(out), DIMENSION(:,:) :: vnew
    REAL(go_wp), intent(in),  DIMENSION(:,:) :: vold, z, cu, h
    REAL(go_wp), intent(in) :: tdt
    ! Locals
    REAL(go_wp) :: tdts8, tdtsdy

    !> These quantities are computed here because tdt is not
    !! constant. (It is == dt for first time step, 2xdt for
    !! all remaining time steps.)
    tdts8 = tdt/8.0d0
    tdtsdy = tdt/dy

    VNEW(I,J) = VOLD(I,J)- &
                TDTS8*(Z(I+1,J)+Z(I,J)) & 
                *(CU(I+1,J)+CU(I,J)+CU(I,J-1)+CU(I+1,J-1)) & 
                 -TDTSDY*(H(I,J)-H(I,J-1))

  END SUBROUTINE compute_vnew_code

END MODULE compute_vnew_mod
