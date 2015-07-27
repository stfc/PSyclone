!> \brief Compute the mass flux in the x direction, cu
!! \detail Given the current pressure and velocity fields,
!! computes the mass flux in the x direction.
module compute_cu_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  implicit none

  private

  public manual_invoke_compute_cu
  public compute_cu_type, compute_cu_code

  type, extends(kernel_type) :: compute_cu_type
     type(arg), dimension(3) :: meta_args =    &
          (/ arg(WRITE, CU, POINTWISE),        & ! cu
             arg(READ,  CT, POINTWISE),        & ! p
             arg(READ,  CU, POINTWISE)         & ! u
           /)
     !> We only have one value per grid point and that means
     !! we have a single DOF per grid point.
     integer :: ITERATES_OVER = DOFS
  contains
    procedure, nopass :: code => compute_cu_code
  end type compute_cu_type

contains

  !===================================================

  !> Manual implementation of the code needed to invoke
  !! compute_cu_code().
  subroutine manual_invoke_compute_cu(cufld, p, u)
    use topology_mod, only: cu
    implicit none
    real(wp), intent(out), dimension(:,:) :: cufld
    real(wp), intent(in),  dimension(:,:) :: p, u
    ! Locals
    integer :: I, J

    ! Note that we do not loop over the full extent of the field.
    ! Fields are allocated with extents (M+1,N+1).
    ! Presumably the extra row and column are needed for periodic BCs.
    ! We are updating a quantity on CU.
    ! This loop writes to cu(2:M+1,1:N) so this looks like
    ! (using x to indicate a location that is written):
    !
    ! i=1   i=M
    !  o  o  o  o 
    !  o  x  x  x   j=N
    !  o  x  x  x
    !  o  x  x  x   j=1

    ! Quantity CU is mass flux in x direction.

    ! Original code looked like:
    !
    !    DO J=1,N
    !      DO I=1,M
    !           CU(I+1,J) = .5*(P(I+1,J)+P(I,J))*U(I+1,J)
    !      END DO
    !    END DO

    ! cu(i,j) depends upon:
    !   p(i-1,j), p(i,j) : CT
    !    => lateral CT neighbours of the CU pt being updated
    !   u(i,j)           : CU
    !    => the horiz. vel. component at the CU pt being updated

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

    do J=cu%jstart, cu%jstop
       do I=cu%istart, cu%istop

          call compute_cu_code(i, j, cufld, p, u)
       end do
    end do

  end subroutine manual_invoke_compute_cu

  !===================================================

  !> Compute the mass flux in the x direction at point (i,j)
  subroutine compute_cu_code(i, j, cu, p, u)
    implicit none
    integer,  intent(in) :: I, J
    real(wp), intent(out), dimension(:,:) :: cu
    real(wp), intent(in),  dimension(:,:) :: p, u

    CU(I,J) = .5*(P(I,J)+P(I-1,J))*U(I,J)

  end subroutine compute_cu_code

end module compute_cu_mod
