module kernel_sw_offset_ct_mod
  use argument_mod
  use field_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod
  implicit none

  private

  public compute_h, compute_h_code
  public apply_bcs_h, apply_bcs_h_code

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

  type, extends(kernel_type) :: apply_bcs_h
     type(go_arg), dimension(4) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CT, GO_POINTWISE),        & ! h
             go_arg(GO_READ,  GO_CT, GO_POINTWISE),        & ! p
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        & ! u
             go_arg(GO_READ,  GO_CV, GO_POINTWISE)         & ! v
           /)
     !> This kernel writes to all points of the
     !! simulation domain.
     integer :: ITERATES_OVER = GO_ALL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the South and West of it.
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => apply_bcs_h_code
  end type apply_bcs_h


contains

  !===================================================

  SUBROUTINE compute_h_code(i, j, h, p, u, v)
    IMPLICIT none
    integer,  intent(in) :: I, J
    REAL(go_wp), INTENT(out), DIMENSION(:,:) :: h
    REAL(go_wp), INTENT(in),  DIMENSION(:,:) :: p, u, v

    H(I,J) = P(I,J)+.25d0*(U(I+1,J)*U(I+1,J)+U(I,J)*U(I,J) + & 
                           V(I,J+1)*V(I,J+1)+V(I,J)*V(I,J))

  END SUBROUTINE compute_h_code

  !===================================================

  SUBROUTINE apply_bcs_h_code(i, j, h, p, u, v)
    IMPLICIT none
    integer,  intent(in) :: I, J
    REAL(go_wp), INTENT(out), DIMENSION(:,:) :: h
    REAL(go_wp), INTENT(in),  DIMENSION(:,:) :: p, u, v

    H(I,J) = P(I,J)+.25d0*(U(I,J)*U(I,J)+U(I,J)*U(I,J) + & 
                           V(I,J)*V(I,J)+V(I,J)*V(I,J))

  END SUBROUTINE apply_bcs_h_code

END MODULE kernel_sw_offset_ct_mod
