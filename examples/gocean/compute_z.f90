!> \brief Compute the potential vorticity, z
!! \detail Given the current pressure and velocity fields,
!! computes the potential voriticity.
module compute_z
  use kind_params
  use kernel_mod
  use argument_mod
  implicit none

  private

  public manual_invoke_compute_z
  public compute_z_type, compute_z_code

  type, extends(kernel_type) :: compute_z_type
     type(arg), dimension(4) :: meta_args =    &
          (/ arg(WRITE, CT, POINTWISE),        & ! z
             arg(READ,  CT, POINTWISE),        & ! p
             arg(READ,  CU, POINTWISE),        & ! u
             arg(READ,  CV, POINTWISE)         & ! v
           /)
     !> We only have one value per grid point and that means
     !! we have a single DOF per grid point.
     integer :: ITERATES_OVER = DOFS
  contains
    procedure, nopass :: code => compute_z_code
  end type compute_z_type

contains

  !===================================================

  !> Manual implementation of the code needed to invoke
  !! compute_z_code().
  subroutine manual_invoke_compute_z(z, p, u, v)
    implicit none
    real(wp), intent(out), dimension(:,:) :: z
    real(wp), intent(in),  dimension(:,:) :: p, u, v
    ! Locals
    integer :: I, J

    ! Note that we do not loop over the full extent of the field.
    ! Fields are allocated with extents (M+1,N+1).
    ! Presumably the extra row and column are needed for periodic BCs.
    ! We are updating a quantity on CF.
    ! This loop writes to z(2:M+1,2:N+1) so this looks like
    ! (using x to indicate a location that is written):
    !
    ! i=1   i=M
    !  o  x  x  x 
    !  o  x  x  x   j=N
    !  o  x  x  x
    !  o  o  o  o   j=1

    ! Quantity Z is Potential Enstrophy

    ! Original code looked like:
    !
    !   DO J=1,N
    !      DO I=1,M
    !         Z(I+1,J+1) =(FSDX*(V(I+1,J+1)-V(I,J+1))-FSDY*(U(I+1,J+1) & 
    !                -U(I+1,J)))/(P(I,J)+P(I+1,J)+P(I+1,J+1)+P(I,J+1))
    !      END DO
    !    END DO

    ! z(i,j) depends upon:
    !   p(i-1,j-1), p(i,j-1), p(i-1,j), p(i,j) : CT
    !    => all CT neighbours of the CF pt being updated
    !   u(i,j), u(i,j-1)                      : CU
    !    => vertical CU neighbours of the CF pt being updated 
    !   v(i,j), v(i-1,j)                      : CV
    !    => lateral CV neighbours of the CF pt being updated

    !   vi-1j+1--fij+1---vij+1---fi+1j+1
    !   |        |       |       |
    !   |        |       |       |
    !   Ti-1j----uij-----Tij-----ui+1j
    !   |        |       |       |
    !   |        |       |       |
    !   vi-1j----fij-----vij-----fi+1j
    !   |        |       |       |
    !   |        |       |       |
    !   Ti-1j-1--uij-1---Tij-1---ui+1j-1
    !

    do J=2, size(z, 2)
       do I=2, size(z, 1)

          call compute_z_code(i, j, z, p, u, v)
       end do
    end do

  end subroutine manual_invoke_compute_z

  !===================================================

  !> Compute the potential vorticity on the grid point (i,j)
  subroutine compute_z_code(i, j, z, p, u, v)
    use mesh, only: fsdx, fsdy
    implicit none
    integer,  intent(in) :: I, J
    real(wp), intent(out), dimension(:,:) :: z
    real(wp), intent(in),  dimension(:,:) :: p, u, v

    Z(I,J) =(FSDX*(V(I,J)-V(I-1,J))-FSDY*(U(I,J)-U(I,J-1)))/ &
                 (P(I-1,J-1)+P(I,J-1)+P(I,J)+P(I-1,J))

  end subroutine compute_z_code

end module compute_z
