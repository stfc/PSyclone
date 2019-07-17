MODULE alg
  CONTAINS
  SUBROUTINE do_update(fld1, fld2)
    USE psy_alg, ONLY: invoke_0
    USE field_mod, ONLY: r2d_field
    USE kern_use_var_mod, ONLY: kern_use_var
    USE kern_call_kern_mod, ONLY: kern_call_kern
    USE kern_nested_use_mod, ONLY: kern_nested_use
    IMPLICIT NONE
    TYPE(r2d_field), INTENT(INOUT) :: fld1, fld2
    CALL invoke_0(fld1, fld2)
  END SUBROUTINE do_update
END MODULE alg