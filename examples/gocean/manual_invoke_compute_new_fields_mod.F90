MODULE manual_invoke_compute_new_fields_mod
  USE kind_params
  IMPLICIT none
  PRIVATE

  PUBLIC manual_invoke_compute_new_fields

CONTAINS

  SUBROUTINE manual_invoke_compute_new_fields(unew, uold, &
                                              vnew, vold, &
                                              pnew, pold, &
                                              z, cu, cv, h, tdt)
    USE compute_unew, ONLY: compute_unew_code
    USE compute_vnew, ONLY: compute_vnew_code
    USE compute_pnew, ONLY: compute_pnew_code
    IMPLICIT none
    REAL(wp), INTENT(out), DIMENSION(:,:) :: unew, vnew, pnew
    REAL(wp), INTENT(in),  DIMENSION(:,:) :: uold, vold, pold
    REAL(wp), INTENT(in),  DIMENSION(:,:) :: z, cu, cv, h
    REAL(wp), INTENT(in) :: tdt
    ! Locals
    integer :: i, j
    integer :: m, n

    m = size(z,1) - 1
    n = size(z,2) - 1

    !CALL manual_invoke_compute_unew(unew, uold,  z, cv, h, tdt)
    DO J=1, N !SIZE(z, 2) - 1
       DO I=2, M+1 !SIZE(z, 1)

          CALL compute_unew_code(i, j, unew, uold, &
                                 z, cv, h, tdt)

       END DO
    END DO
    !CALL manual_invoke_compute_vnew(vnew, vold,  z, cu, h, tdt)
    DO J=2, N+1 !SIZE(z, 2)
       DO I=1, M !SIZE(z, 1) - 1

          CALL compute_vnew_code(i, j, vnew, vold, &
                                 z, cu, h, tdt)
       END DO
    END DO
    !CALL manual_invoke_compute_pnew(pnew, pold, cu, cv,    tdt)
    DO J=1, N !SIZE(z, 2) - 1
       DO I=1, M !SIZE(z, 1) - 1

          CALL compute_pnew_code(i, j, pnew, pold, &
                                 cu, cv, tdt)
       END DO
    END DO

  END SUBROUTINE manual_invoke_compute_new_fields

END MODULE manual_invoke_compute_new_fields_mod
