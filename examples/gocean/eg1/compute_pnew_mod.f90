module compute_pnew_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod
  use field_mod
  implicit none

  private

  public compute_pnew, compute_pnew_code

  type, extends(kernel_type) :: compute_pnew
     type(go_arg), dimension(7) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CT, GO_POINTWISE),            & ! pnew
             go_arg(GO_READ,  GO_CT, GO_POINTWISE),            & ! pold
             go_arg(GO_READ,  GO_CU, GO_STENCIL(000,011,000)), & ! cu
             go_arg(GO_READ,  GO_CV, GO_STENCIL(010,010,000)), & ! cv
             go_arg(GO_READ,  GO_R_SCALAR, GO_POINTWISE),  & ! tdt
             go_arg(GO_READ,  GO_GRID_DX_CONST),        & ! dx
             go_arg(GO_READ,  GO_GRID_DY_CONST)         & ! dy
           /)
     !> This kernel operates on fields that live on an
     !! orthogonal, regular grid.
     integer :: GRID_TYPE = GO_ORTHOGONAL_REGULAR

     !> This kernel writes only to internal grid points
     INTEGER :: ITERATES_OVER = GO_INTERNAL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the South and West of it.
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_pnew_code
  end type compute_pnew

contains

  !===================================================

  subroutine compute_pnew_code(i, j, &
                               pnew, pold, cu, cv, &
                               tdt, dx, dy)
    implicit none
    integer,  intent(in) :: I, J
    real(go_wp), intent(in) :: dx, dy
    real(go_wp), intent(out), dimension(:,:) :: pnew
    real(go_wp), intent(in),  dimension(:,:) :: pold, cu, cv
    real(go_wp), intent(in) :: tdt
    ! Locals
    real(go_wp) :: tdtsdx, tdtsdy

    !> These quantities are computed here because tdt is not
    !! constant. (It is == dt for first time step, 2xdt for
    !! all remaining time steps.)
    tdtsdx = tdt/dx
    tdtsdy = tdt/dy

    PNEW(I,J) = POLD(I,J)-TDTSDX*(CU(I+1,J)-CU(I,J))   & 
                         -TDTSDY*(CV(I,J+1)-CV(I,J))

  END SUBROUTINE compute_pnew_code

END MODULE compute_pnew_mod
