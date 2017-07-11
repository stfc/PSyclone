module kernel_sw_offset_ct_mod
  implicit none

  private

  public compute_h, compute_h_code
  public apply_bcs_h, apply_bcs_h_code

  type, extends(kernel_type) :: compute_h
     type(arg), dimension(4) :: meta_args =    &
          (/ arg(WRITE, CT, POINTWISE),        & ! h
             arg(READ,  CT, POINTWISE),        & ! p
             arg(READ,  CU, POINTWISE),        & ! u
             arg(READ,  CV, POINTWISE)         & ! v
           /)
     !> This kernel writes only to internal points of the
     !! simulation domain.
     integer :: ITERATES_OVER = INTERNAL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the South and West of it.
     integer :: index_offset = OFFSET_SW

  contains
    procedure, nopass :: code => compute_h_code
  end type compute_h

  type, extends(kernel_type) :: apply_bcs_h
     type(arg), dimension(4) :: meta_args =    &
          (/ arg(WRITE, CT, POINTWISE),        & ! h
             arg(READ,  CT, POINTWISE),        & ! p
             arg(READ,  CU, POINTWISE),        & ! u
             arg(READ,  CV, POINTWISE)         & ! v
           /)
     !> This kernel writes to all points of the
     !! simulation domain.
     integer :: ITERATES_OVER = ALL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the South and West of it.
     integer :: index_offset = OFFSET_SW

  contains
    procedure, nopass :: code => apply_bcs_h_code
  end type apply_bcs_h


contains

  !===================================================

  SUBROUTINE compute_h_code(i, j, h, p, u, v)
    IMPLICIT none
    integer,  intent(in) :: I, J
    REAL(wp), INTENT(out), DIMENSION(:,:) :: h
    REAL(wp), INTENT(in),  DIMENSION(:,:) :: p, u, v

    H(I,J) = P(I,J)+.25d0*(U(I+1,J)*U(I+1,J)+U(I,J)*U(I,J) + & 
                           V(I,J+1)*V(I,J+1)+V(I,J)*V(I,J))

  END SUBROUTINE compute_h_code

  !===================================================

  SUBROUTINE apply_bcs_h_code(i, j, h, p, u, v)
    IMPLICIT none
    integer,  intent(in) :: I, J
    REAL(wp), INTENT(out), DIMENSION(:,:) :: h
    REAL(wp), INTENT(in),  DIMENSION(:,:) :: p, u, v

    H(I,J) = P(I,J)+.25d0*(U(I,J)*U(I,J)+U(I,J)*U(I,J) + & 
                           V(I,J)*V(I,J)+V(I,J)*V(I,J))

  END SUBROUTINE apply_bcs_h_code

END MODULE kernel_sw_offset_ct_mod
