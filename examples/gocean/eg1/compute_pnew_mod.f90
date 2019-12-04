module compute_pnew_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod
  use field_mod
  implicit none

  private

  public invoke_compute_pnew
  public compute_pnew, compute_pnew_code

  type, extends(kernel_type) :: compute_pnew
     type(go_arg), dimension(7) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CT, GO_POINTWISE),        & ! pnew
             go_arg(GO_READ,  GO_CT, GO_POINTWISE),        & ! pold
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        & ! cu
             go_arg(GO_READ,  GO_CV, GO_POINTWISE),        & ! cv
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

  subroutine invoke_compute_pnew(pnew, pold, cu, cv, tdt)
    implicit none
    type(r2d_field), intent(inout) :: pnew
    type(r2d_field), intent(in)    :: pold, cu, cv
    real(go_wp), intent(in) :: tdt
    ! Locals
    integer :: I, J
    real(go_wp) :: dx, dy

    ! Note that we do not loop over the full extent of the field.
    ! Fields are allocated with extents (M+1,N+1).
    ! Presumably the extra row and column are needed for periodic BCs.
    ! We are updating a quantity on CT.
    ! This loop writes to pnew(1:M,1:N) so this looks like
    ! (using x to indicate a location that is written):
    !
    ! i=1   i=M
    !  o  o  o  o 
    !  x  x  x  o   j=N
    !  x  x  x  o
    !  x  x  x  o   j=1

    ! Original code looked like:
    !
    !     DO J=1,N
    !        DO I=1,M
    !           PNEW(I,J) = POLD(I,J)-TDTSDX*(CU(I+1,J)-CU(I,J))   & 
    !               -TDTSDY*(CV(I,J+1)-CV(I,J))
    !        END DO
    !     END DO

    ! pnew(i,j) depends upon:
    !   pold(i,j)                                : CT
    !   cu(i,j), cu(i+1,j)                       : CU
    !    => lateral CU neighbours of the CT pt being updated 
    !   cv(i,j), cv(i,j+1)                       : CT
    !    => vertical CV neighbours of the CT pt being updated

    !   x-------vij+1---fi+1j+1
    !   |       |       |
    !   |       |       |
    !   uij-----Tij-----ui+1j
    !   |       |       |
    !   |       |       |
    !   fij-----vij-----fi+1j
    !   |       |       |
    !   |       |       |
    !   uij-1- -Tij-1---ui+1j-1
    !
    dx = pnew%grid%dx
    dy = pnew%grid%dy

    DO J=pnew%internal%ystart, pnew%internal%ystop, 1
       DO I=pnew%internal%xstart, pnew%internal%xstop, 1

          CALL compute_pnew_code(i, j,                 &
                                 pnew%data, pold%data, &
                                 cu%data, cv%data, tdt, dx, dy)
       END DO
    END DO

  end subroutine invoke_compute_pnew

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
