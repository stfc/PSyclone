!> \brief A fake kernel that assumes a NE offset and 
!! updates a field on T points
module compute_vort_mod
  implicit none

  private

  public compute_vort, compute_vort_code

  type, extends(kernel_type) :: compute_vort
     type(arg), dimension(3) :: meta_args =    &
          (/ arg(WRITE, CT, POINTWISE),        &
             arg(READ,  CU, POINTWISE),        &
             arg(READ,  CV, POINTWISE)         &
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
     !! to the North and East of it.
     integer :: index_offset = OFFSET_NE

  contains
    procedure, nopass :: code => compute_vort_code
  end type compute_vort

contains

  !===================================================

  subroutine compute_vort_code(i, j, p, cu, cv)
    implicit none
    integer,  intent(in) :: I, J
    real(wp), intent(out), dimension(:,:) :: vort
    real(wp), intent(in),  dimension(:,:) :: p, cu, cv

    ! This is a FAKE kernel - it doesn't do anything!
    p(I,J) = .5d0**cu(I,J)*cv(i,j)

  end subroutine compute_vort_code

end module compute_vort_mod
