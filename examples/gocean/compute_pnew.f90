MODULE compute_pnew
  USE kind_params
  USE kernel_mod
  use argument_mod
  IMPLICIT none

  PRIVATE

  PUBLIC manual_invoke_compute_pnew
  PUBLIC compute_pnew_type, compute_pnew_code

  TYPE, EXTENDS(kernel_type) :: compute_pnew_type
     TYPE(arg), DIMENSION(5) :: meta_args =    &
          (/ arg(WRITE, CT, POINTWISE),        & ! pnew
             arg(READ,  CT, POINTWISE),        & ! pold
             arg(READ,  CU, POINTWISE),        & ! cu
             arg(READ,  CV, POINTWISE),        & ! cv
             arg(READ,  R,  POINTWISE)         & ! tdt
           /)
     !> We only have one value per grid point and that means
     !! we have a single DOF per grid point.
     INTEGER :: ITERATES_OVER = DOFS
  CONTAINS
    procedure, nopass :: code => compute_pnew_code
  END TYPE compute_pnew_type

CONTAINS

  !===================================================

  SUBROUTINE manual_invoke_compute_pnew(pnew, pold, cu, cv, tdt)
    IMPLICIT none
    REAL(wp), INTENT(out), DIMENSION(:,:) :: pnew
    REAL(wp), INTENT(in),  DIMENSION(:,:) :: pold, cu, cv
    REAL(wp), INTENT(in) :: tdt
    ! Locals
    INTEGER :: I, J

    ! Note that we do not loop over the full extent of the field.
    ! Fields are allocated with extents (M+1,N+1).
    ! Presumably the extra row and column are needed for periodic BCs.
    ! We are updating a quantity on CT.
    ! This loop writes to pnew(1:M,1:N) so this looks like
    ! (using x to indicate a location that is written):
    !
    ! i=1   i=M
    !  o  o  o  o 
    !  x  x  x  o   j=N
    !  x  x  x  o
    !  x  x  x  o   j=1

    ! Original code looked like:
    !
    !     DO J=1,N
    !        DO I=1,M
    !           PNEW(I,J) = POLD(I,J)-TDTSDX*(CU(I+1,J)-CU(I,J))   & 
    !               -TDTSDY*(CV(I,J+1)-CV(I,J))
    !        END DO
    !     END DO

    ! pnew(i,j) depends upon:
    !   pold(i,j)                                : CT
    !   cu(i,j), cu(i+1,j)                       : CU
    !    => lateral CU neighbours of the CT pt being updated 
    !   cv(i,j), cv(i,j+1)                       : CT
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

    DO J=1,SIZE(pnew, 2) - 1
       DO I=1,SIZE(pnew, 1) - 1

          CALL compute_pnew_code(i, j, pnew, pold, &
                                 cu, cv, tdt)
       END DO
    END DO

  END SUBROUTINE manual_invoke_compute_pnew

  !===================================================

  SUBROUTINE compute_pnew_code(i, j, pnew, pold, cu, cv, tdt)
    USE model, ONLY: dx, dy
    IMPLICIT none
    INTEGER, INTENT(in) :: I, J
    REAL(wp), INTENT(out), DIMENSION(:,:) :: pnew
    REAL(wp), INTENT(in),  DIMENSION(:,:) :: pold, cu, cv
    REAL(wp), INTENT(in) :: tdt
    ! Locals
    REAL(wp) :: tdtsdx, tdtsdy
   
    !> These quantities are computed here because tdt is not
    !! constant. (It is == dt for first time step, 2xdt for
    !! all remaining time steps.)
    tdtsdx = tdt/dx
    tdtsdy = tdt/dy

    PNEW(I,J) = POLD(I,J)-TDTSDX*(CU(I+1,J)-CU(I,J))   & 
                         -TDTSDY*(CV(I,J+1)-CV(I,J))

  END SUBROUTINE compute_pnew_code

END MODULE compute_pnew
