!> \brief Compute the mass flux in the y direction, cv
!! \detail Given the current pressure and velocity fields,
!! computes the mass flux in the y direction.
module compute_cv
  use kind_params
  use kernel_mod
  use argument_mod
  implicit none

  private

  public manual_invoke_compute_cv
  public compute_cv_type, compute_cv_code

  type, extends(kernel_type) :: compute_cv_type
     type(arg), dimension(3) :: meta_args =    &
          (/ arg(WRITE, CV, POINTWISE),        & ! cv
             arg(READ,  CT, POINTWISE),        & ! p
             arg(READ,  CV, POINTWISE)         & ! v
           /)
     !> We only have one value per grid point and that means
     !! we have a single DOF per grid point.
     integer :: ITERATES_OVER = DOFS
  contains
    procedure, nopass :: code => compute_cv_code
  end type compute_cv_type

contains

  !===================================================

  !> Manual implementation of the code needed to invoke
  !! compute_cv_code().
  subroutine manual_invoke_compute_cv(cv, p, v)
    implicit none
    real(wp), intent(out), dimension(:,:) :: cv
    real(wp), intent(in),  dimension(:,:) :: p, v
    ! Locals
    integer :: I, J

    ! Note that we do not loop over the full extent of the field.
    ! Fields are allocated with extents (M+1,N+1).
    ! Presumably the extra row and column are needed for periodic BCs.
    ! We are updating a quantity on CV.
    ! This loop writes to cv(1:M,2:N+1) so this looks like
    ! (using x to indicate a location that is written):
    !
    ! i=1   i=M
    !  x  x  x  o 
    !  x  x  x  o   j=N
    !  x  x  x  o
    !  o  o  o  o   j=1

    ! Quantity CV is mass flux in y direction.

    ! Original code looked like:
    !
    !    DO J=1,N
    !      DO I=1,M
    !           CV(I,J+1) = .5*(P(I,J+1)+P(I,J))*V(I,J+1)
    !      END DO
    !    END DO

    ! cv(i,j) depends upon:
    !   p(i,j-1), p(i,j) : CT
    !    => vertical CT neighbours of the CV pt being updated
    !   v(i,j)           : CV
    !    => the velocity component at the CV pt being updated

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

    do J=2, size(cv, 2)
       do I=1, size(cv, 1) - 1

          call compute_cv_code(i, j, cv, p, v)
       end do
    end do

  end subroutine manual_invoke_compute_cv

  !===================================================

  !> Compute the mass flux in the y direction at point (i,j)
  subroutine compute_cv_code(i, j, cv, p, v)
    implicit none
    integer,  intent(in) :: I, J
    real(wp), intent(out), dimension(:,:) :: cv
    real(wp), intent(in),  dimension(:,:) :: p, v

    CV(I,J) = .5*(P(I,J)+P(I,J-1))*V(I,J)

  end subroutine compute_cv_code

end module compute_cv
