!> \brief Compute the mass flux in the y direction, cv
!! \detail Given the current pressure and velocity fields,
!! computes the mass flux in the y direction.
module compute_cv_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod
  use field_mod
  implicit none

  private

  public compute_cv, compute_cv_code

  type, extends(kernel_type) :: compute_cv
     type(go_arg), dimension(3) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CV, GO_POINTWISE),            & ! cv
             go_arg(GO_READ,  GO_CT, GO_STENCIL(000,010,010)), & ! p
             go_arg(GO_READ,  GO_CV, GO_POINTWISE)             & ! v
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
    procedure, nopass :: code => compute_cv_code
  end type compute_cv

contains

  !===================================================

  !> Compute the mass flux in the y direction at point (i,j)
  subroutine compute_cv_code(i, j, cv, p, v)
    implicit none
    integer,  intent(in) :: I, J
    real(go_wp), intent(out), dimension(:,:) :: cv
    real(go_wp), intent(in),  dimension(:,:) :: p, v

    CV(I,J) = .5d0*(P(I,J)+P(I,J-1))*V(I,J)

  end subroutine compute_cv_code

end module compute_cv_mod
