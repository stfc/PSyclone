!> \brief Compute the potential vorticity, z
!! \detail Given the current pressure and velocity fields,
!! computes the potential voriticity.
module kernel_sw_offset_cf_mod
  implicit none

  private

  public compute_z, compute_z_code

  type, extends(kernel_type) :: compute_z
     type(arg), dimension(6) :: meta_args =    &
          (/ arg(WRITE, CF, POINTWISE),        & ! z
             arg(READ,  CT, POINTWISE),        & ! p
             arg(READ,  CU, POINTWISE),        & ! u
             arg(READ,  CV, POINTWISE),        & ! v
             arg(READ,  GRID_DX_CONST),        & ! dx
             arg(READ,  GRID_DY_CONST)         & ! dy
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
     procedure, nopass :: code => compute_z_code
  end type compute_z

contains

  !===================================================

  !> Compute the potential vorticity on the grid point (i,j)
  subroutine compute_z_code(i, j, z, p, u, v, dx, dy)
    implicit none
    integer,  intent(in) :: I, J
    real(wp), intent(in) :: dx, dy
    real(wp), intent(inout), dimension(:,:) :: z
    real(wp), intent(in),  dimension(:,:) :: p, u, v

    Z(I,J) =( (4.0d0/dx)*( V(I,J)-V(I-1,J))-    &
              (4.0d0/dy)*( U(I,J)-U(I,J-1)) ) / &
            (P(I-1,J-1)+P(I,J-1)+ P(I,J)+P(I-1,J))

  end subroutine compute_z_code

end module kernel_sw_offset_cf_mod
