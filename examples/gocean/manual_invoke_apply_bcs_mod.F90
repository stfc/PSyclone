MODULE manual_invoke_apply_bcs_mod
  USE kind_params
  IMPLICIT none
  PRIVATE

  PUBLIC manual_invoke_apply_bcs_uvtf

CONTAINS

  SUBROUTINE manual_invoke_apply_bcs_uvtf(ufield, vfield, tfield, ffield)
    USE apply_bcs_cf, ONLY: apply_bcs_cf_code
    USE apply_bcs_ct, ONLY: apply_bcs_ct_code
    USE apply_bcs_cu, ONLY: apply_bcs_cu_code
    USE apply_bcs_cv, ONLY: apply_bcs_cv_code
    IMPLICIT none
    REAL(wp), INTENT(inout), DIMENSION(:,:) :: ufield, vfield, tfield, ffield
    ! Locals
    integer :: m, n, mp1, np1

    MP1 = SIZE(ufield, 1)
    NP1 = SIZE(ufield, 2)
    M = MP1 - 1
    N = NP1 - 1

    call apply_bcs_cu_code(n, mp1, np1, ufield)
    call apply_bcs_ct_code(n, mp1, np1, tfield)
    call apply_bcs_cv_code(m, mp1, np1, vfield)
    call apply_bcs_cf_code(mp1, np1, ffield)

  END SUBROUTINE manual_invoke_apply_bcs_uvtf

END MODULE manual_invoke_apply_bcs_mod
