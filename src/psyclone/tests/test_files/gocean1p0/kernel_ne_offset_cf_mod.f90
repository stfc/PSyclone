!> \brief A fake kernel that assumes a NE offset and 
!! updates a field on F points
module kernel_ne_offset_cf_mod
  use argument_mod
  use field_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod
  implicit none

  private

  public compute_vort, compute_vort_code

  type, extends(kernel_type) :: compute_vort
     type(go_arg), dimension(4) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CF, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CT, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CV, GO_POINTWISE)         &
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
     !! to the North and East of it.
     integer :: index_offset = GO_OFFSET_NE

  contains
    procedure, nopass :: code => compute_vort_code
  end type compute_vort

contains

  !===================================================

  subroutine compute_vort_code(i, j, vort, p, cu, cv)
    implicit none
    integer,  intent(in) :: I, J
    real(go_wp), intent(out), dimension(:,:) :: vort
    real(go_wp), intent(in),  dimension(:,:) :: p, cu, cv

    ! This is a FAKE kernel - it doesn't do anything!
    vort(I,J) = .5d0*(P(I,J+1)+P(I,J))*cu(I,J)*cv(i,j)

  end subroutine compute_vort_code

end module kernel_ne_offset_cf_mod
