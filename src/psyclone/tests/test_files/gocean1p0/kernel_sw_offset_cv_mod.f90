!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. Porter STFC Daresbury Lab
! Funded by the NERC GOcean project
module kernel_sw_offset_cv_mod
  implicit none

  private

  public compute_v, compute_v_code
  public apply_bcs_v, apply_bcs_v_code

  type, extends(kernel_type) :: compute_v
     type(arg), dimension(3) :: meta_args =    &
          (/ arg(WRITE, CU, POINTWISE),        &
             arg(READ,  CV, POINTWISE),        &
             arg(READ,  CT, POINTWISE)         &
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
    procedure, nopass :: code => compute_v_code
  end type compute_v

  type, extends(kernel_type) :: apply_bcs_v
     type(arg), dimension(2) :: meta_args =    &
          (/ arg(WRITE, CV, POINTWISE),        &
             arg(READ,  CU, POINTWISE)         &
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
    procedure, nopass :: code => apply_bcs_v_code
  end type apply_bcs_v

contains

  !===================================================

  SUBROUTINE compute_v_code(i, j, v, u, h)
    IMPLICIT none
    integer,  intent(in) :: I, J
    REAL(wp), INTENT(inout), DIMENSION(:,:) :: v
    REAL(wp), INTENT(in),    DIMENSION(:,:) :: u, h

    v(I,J) = h(I,J)+.25d0*(U(I+1,J)*U(I+1,J)+U(I,J)*U(I,J) + & 
                           V(I,J+1)*V(I,J+1)+V(I,J)*V(I,J))

  END SUBROUTINE compute_v_code

  !===================================================

  SUBROUTINE apply_bcs_v_code(i, j, v, u)
    IMPLICIT none
    integer,  intent(in) :: I, J
    REAL(wp), INTENT(inout), DIMENSION(:,:) :: v
    REAL(wp), INTENT(in),    DIMENSION(:,:) :: u

    v(I,J) = .25d0*(U(I,J)*U(I,J)+U(I,J)*U(I,J) + & 
              V(I,J)*V(I,J)+V(I,J)*V(I,J))

  END SUBROUTINE apply_bcs_v_code

END MODULE kernel_sw_offset_cv_mod
