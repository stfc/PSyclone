MODULE compute_h
  USE kind_params
  USE kernel_mod
  use argument_mod
  IMPLICIT none

  PRIVATE

  PUBLIC manual_invoke_compute_h
  PUBLIC compute_h_type, compute_h_code

  TYPE, EXTENDS(kernel_type) :: compute_h_type
     TYPE(arg), DIMENSION(4) :: meta_args =    &
          (/ arg(WRITE, CT, POINTWISE),        & ! h
             arg(READ,  CT, POINTWISE),        & ! p
             arg(READ,  CU, POINTWISE),        & ! u
             arg(READ,  CV, POINTWISE)         & ! v
           /)
     !> We only have one value per grid point and that means
     !! we have a single DOF per grid point.
     INTEGER :: ITERATES_OVER = DOFS
  CONTAINS
    procedure, nopass :: code => compute_h_code
  END TYPE compute_h_type

CONTAINS

  !===================================================

  SUBROUTINE manual_invoke_compute_h(h, p, u, v)
    IMPLICIT none
    REAL(wp), INTENT(out), DIMENSION(:,:) :: h
    REAL(wp), INTENT(in),  DIMENSION(:,:) :: p, u,v
    ! Locals
    INTEGER :: I, J

    ! Note that we do not loop over the full extent of the field.
    ! Fields are allocated with extents (M+1,N+1).
    ! Presumably the extra row and column are needed for periodic BCs.
    ! We are updating a quantity on CT.
    ! This loop writes to h(1:M,1:N) so this looks like
    ! (using x to indicate a location that is written):
    !
    ! i=1   i=M
    !  o  o  o  o 
    !  x  x  x  o   j=N
    !  x  x  x  o
    !  x  x  x  o   j=1

    ! Quantity H is defined as:
    !         H = P + 0.5(<u^2>_x + <v^2>_y)
    ! where <d>_x indicates average over field d in x direction.

    ! Original code looked like:
    !
    !     DO J=1,N
    !        DO I=1,M
    !           H(I,J) = P(I,J)+.25*(U(I+1,J)*U(I+1,J)+U(I,J)*U(I,J)     & 
    !                               +V(I,J+1)*V(I,J+1)+V(I,J)*V(I,J))
    !        END DO
    !     END DO

    ! h(i,j) depends upon:
    !   p(i,j)                                : CT
    !   u(i,j), u(i+1,j)                      : CU
    !    => lateral CU neighbours of the CT pt being updated 
    !   v(i,j), v(i,j+1)                      : CV
    !    => vertical CV neighbours of the CT pt being updated

    !   x-------vij+1---fi+1j+1
    !   |       |       |
    !   |       |       |
    !   uij-----Tij-----ui+1j
    !   |       |       |
    !   |       |       |
    !   fij-----vij-----fi+1j
    !   |       |       |
    !   |       |       |
    !   uij-1- -Tij-1---ui+1j-1
    !

    DO J=1, SIZE(h, 2) - 1
       DO I=1, SIZE(h, 1) - 1

          CALL compute_h_code(i, j, h, p, u, v)
       END DO
    END DO

  END SUBROUTINE manual_invoke_compute_h

  !===================================================

  SUBROUTINE compute_h_code(i, j, h, p, u, v)
    IMPLICIT none
    INTEGER, INTENT(in) :: I, J
    REAL(wp), INTENT(out), DIMENSION(:,:) :: h
    REAL(wp), INTENT(in),  DIMENSION(:,:) :: p, u, v

    H(I,J) = P(I,J)+.25*(U(I+1,J)*U(I+1,J)+U(I,J)*U(I,J)     & 
                        +V(I,J+1)*V(I,J+1)+V(I,J)*V(I,J))

  END SUBROUTINE compute_h_code

END MODULE compute_h
