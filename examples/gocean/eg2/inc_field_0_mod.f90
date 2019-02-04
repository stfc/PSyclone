MODULE inc_field_0_mod
  USE kind_params_mod
  USE kernel_mod
  USE argument_mod
  USE grid_mod, ONLY: go_offset_sw
  IMPLICIT NONE
  TYPE, EXTENDS(kernel_type) :: inc_field_0_type
    TYPE(go_arg), DIMENSION(4) :: meta_args = (/go_arg(go_write, go_ct, go_pointwise), go_arg(go_read, go_i_scalar, go_pointwise), &
&go_arg(go_read, go_i_scalar, go_pointwise), go_arg(go_read, go_i_scalar, go_pointwise)/)
    INTEGER :: iterates_over = go_internal_pts
    INTEGER :: index_offset = go_offset_sw
    CONTAINS
    PROCEDURE, NOPASS :: code => inc_field_0_code
  END TYPE inc_field_0_type
  CONTAINS
  SUBROUTINE inc_field_0_code(ji, jj, fld1, nx, ny, istp)
    INTEGER, INTENT(IN) :: ji, jj, nx, ny
    REAL(KIND = go_wp), DIMENSION(nx, ny), INTENT(INOUT) :: fld1
    INTEGER, INTENT(IN) :: istp
    !$acc routine
    fld1 (ji, jj) = fld1(ji, jj) + real(istp, go_wp)
  END SUBROUTINE inc_field_0_code
END MODULE inc_field_0_mod