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

  public invoke_compute_cv
  public compute_cv, compute_cv_code

  type, extends(kernel_type) :: compute_cv
     type(go_arg), dimension(3) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CV, GO_POINTWISE),        & ! cv
             go_arg(GO_READ,  GO_CT, GO_POINTWISE),        & ! p
             go_arg(GO_READ,  GO_CV, GO_POINTWISE)         & ! v
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

  !> Manual implementation of the code needed to invoke
  !! compute_cv_code().
  subroutine invoke_compute_cv(cvfld, pfld, vfld)
    implicit none
    type(r2d_field), intent(inout) :: cvfld
    type(r2d_field), intent(in)    :: pfld, vfld
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

    do J=cvfld%internal%ystart, cvfld%internal%ystop
       do I=cvfld%internal%xstart, cvfld%internal%xstop

          call compute_cv_code(i, j, cvfld%data, pfld%data, vfld%data)
       end do
    end do

  end subroutine invoke_compute_cv

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
