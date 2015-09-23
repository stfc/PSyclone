!> \brief A fake kernel that assumes a NE offset and 
!! updates a field on *all* U points
module kernel_ne_offset_all_cu_mod
  implicit none

  private

  public apply_bc_u, apply_bc_u_code

  type, extends(kernel_type) :: apply_bc_u
     type(arg), dimension(2) :: meta_args =    &
          (/ arg(WRITE, CU, POINTWISE),        &
             arg(READ,  CV, POINTWISE)         &
           /)
     !> This kernel writes to all CU points of the
     !! simulation domain.
     integer :: ITERATES_OVER = ALL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the North and East of it.
     integer :: index_offset = OFFSET_NE

  contains
    procedure, nopass :: code => apply_bc_u_code
  end type apply_bc_u

contains

  !===================================================

  subroutine apply_bc_u_code(i, j, cu, cv)
    implicit none
    integer,  intent(in) :: I, J
    real(wp), intent(inout), dimension(:,:) :: cu
    real(wp), intent(in),    dimension(:,:) :: cv

    ! This is a FAKE kernel - it doesn't do anything!
    vort(I,J) = .5d0*(P(I,J+1)+P(I,J))*cu(I,J)*cv(i,j)

  end subroutine apply_bc_u_code

end module kernel_ne_offset_all_cu_mod
