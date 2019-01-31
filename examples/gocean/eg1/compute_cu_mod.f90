!> \brief Compute the mass flux in the x direction, cu
!! \detail Given the current pressure and velocity fields,
!! computes the mass flux in the x direction.
module compute_cu_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use field_mod
  use grid_mod
  implicit none

  private

  public invoke_compute_cu
  public compute_cu, compute_cu_code

  type, extends(kernel_type) :: compute_cu
     type(arg), dimension(3) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),        & ! cu
             go_arg(GO_READ,  GO_CT, GO_POINTWISE),        & ! p
             go_arg(GO_READ,  GO_CU, GO_POINTWISE)         & ! u
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
    procedure, nopass :: code => compute_cu_code
  end type compute_cu

contains

  !===================================================

  !> Manual implementation of the code needed to invoke
  !! compute_cu_code().
  subroutine invoke_compute_cu(cufld, pfld, ufld)
    implicit none
    type(r2d_field), intent(inout) :: cufld
    type(r2d_field), intent(in)    :: pfld, ufld
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

    do J=cufld%internal%ystart, cufld%internal%ystop
       do I=cufld%internal%xstart, cufld%internal%xstop

          call compute_cu_code(i, j, cufld%data, pfld%data, ufld%data)
       end do
    end do

  end subroutine invoke_compute_cu

  !===================================================

  !> Compute the mass flux in the x direction at point (i,j)
  subroutine compute_cu_code(i, j, cu, p, u)
    implicit none
    integer,  intent(in) :: I, J
    real(wp), intent(out), dimension(:,:) :: cu
    real(wp), intent(in),  dimension(:,:) :: p, u


    CU(I,J) = 0.5d0*(P(i,J)+P(I-1,J))*U(I,J)
    
    !write (*,"('CU calc: ',I3,1x,I3,3(1x,E24.16))") &
    !     i, j, p(i,j), p(i-1,j), u(i,j)

  end subroutine compute_cu_code

end module compute_cu_mod
