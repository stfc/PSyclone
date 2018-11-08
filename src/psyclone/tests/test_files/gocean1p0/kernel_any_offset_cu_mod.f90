!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. Porter STFC Daresbury Lab
! Funded by the NERC GOcean project
module kernel_any_offset_cu_mod
  use argument_mod
  use field_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod
  implicit none

  private

  public compute_u, compute_u_code

  type, extends(kernel_type) :: compute_u
     type(go_arg), dimension(3) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CV, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CT, GO_POINTWISE)         &
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
     integer :: index_offset = GO_OFFSET_ANY

  contains
    procedure, nopass :: code => compute_u_code
  end type compute_u

contains

  !===================================================

  SUBROUTINE compute_u_code(i, j, u, v, h)
    IMPLICIT none
    integer,  intent(in) :: I, J
    REAL(go_wp), INTENT(inout), DIMENSION(:,:) :: u
    REAL(go_wp), INTENT(in),    DIMENSION(:,:) :: h, v

    u(I,J) = h(I,J)+.25d0*(U(I+1,J)*U(I+1,J)+U(I,J)*U(I,J) + & 
                           V(I,J+1)*V(I,J+1)+V(I,J)*V(I,J))

  END SUBROUTINE compute_u_code

END MODULE kernel_any_offset_cu_mod
