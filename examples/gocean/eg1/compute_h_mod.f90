module compute_h_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod
  use field_mod
  implicit none

  private

  public invoke_compute_h
  public compute_h, compute_h_code

  type, extends(kernel_type) :: compute_h
     type(go_arg), dimension(4) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CT, GO_POINTWISE),        & ! h
             go_arg(GO_READ,  GO_CT, GO_POINTWISE),        & ! p
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        & ! u
             go_arg(GO_READ,  GO_CV, GO_POINTWISE)         & ! v
           /)
     !> This kernel writes only to internal points of the
     !! simulation domain.
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
    procedure, nopass :: code => compute_h_code
  end type compute_h

contains

  !===================================================

  subroutine invoke_compute_h(hfld, pfld, ufld, vfld)
    implicit none
    type(r2d_field), intent(inout) :: hfld
    type(r2d_field), intent(in)    :: pfld, ufld,vfld
    ! Locals
    integer :: I, J

    ! Note that we do not loop over the full extent of the field.
    ! Fields are allocated with extents (M+1,N+1).
    ! Presumably the extra row and column are needed for periodic BCs.
    ! We are updating a quantity on CT.
    ! This loop writes to h(1:M,1:N) so this looks like
    ! (using x to indicate a location that is written):
    !
    ! i=1   i=M
    !  o  o  o  o 
    !  x  x  x  o   j=N
    !  x  x  x  o
    !  x  x  x  o   j=1

    ! Quantity H is defined as:
    !         H = P + 0.5(<u^2>_x + <v^2>_y)
    ! where <d>_x indicates average over field d in x direction.

    ! Original code looked like:
    !
    !     DO J=1,N
    !        DO I=1,M
    !           H(I,J) = P(I,J)+.25*(U(I+1,J)*U(I+1,J)+U(I,J)*U(I,J)     & 
    !                               +V(I,J+1)*V(I,J+1)+V(I,J)*V(I,J))
    !        END DO
    !     END DO

    ! h(i,j) depends upon:
    !   p(i,j)                                : CT
    !   u(i,j), u(i+1,j)                      : CU
    !    => lateral CU neighbours of the CT pt being updated 
    !   v(i,j), v(i,j+1)                      : CV
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

    DO J=hfld%internal%ystart, hfld%internal%ystop, 1
       DO I=hfld%internal%xstart, hfld%internal%xstop, 1

          CALL compute_h_code(i, j, hfld%data, &
                              pfld%data, ufld%data, vfld%data)
       END DO
    END DO

  end subroutine invoke_compute_h

  !===================================================

  SUBROUTINE compute_h_code(i, j, h, p, u, v)
    IMPLICIT none
    integer,  intent(in) :: I, J
    REAL(wp), INTENT(out), DIMENSION(:,:) :: h
    REAL(wp), INTENT(in),  DIMENSION(:,:) :: p, u, v

    H(I,J) = P(I,J)+.25d0*(U(I+1,J)*U(I+1,J)+U(I,J)*U(I,J) + & 
                           V(I,J+1)*V(I,J+1)+V(I,J)*V(I,J))

  END SUBROUTINE compute_h_code

END MODULE compute_h_mod
