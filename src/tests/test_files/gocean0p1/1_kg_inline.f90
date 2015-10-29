  MODULE psy_single_function
    USE field_mod
    USE topology_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_testkern_type(f1, f2, m1)
      REAL(KIND=wp), intent(inout), dimension(:,:) :: f1, f2, m1
      INTEGER idim1
      INTEGER i
      INTEGER idim2
      INTEGER j
      idim2 = SIZE(m1, 2)
      idim1 = SIZE(m1, 1)
      DO j=1,idim2
        DO i=1,idim1
          CALL testkern_code(i, j, f1, f2, m1)
        END DO 
      END DO 
    END SUBROUTINE invoke_testkern_type
    SUBROUTINE testkern_code(a, b, c)
    END SUBROUTINE testkern_code
  END MODULE psy_single_function
