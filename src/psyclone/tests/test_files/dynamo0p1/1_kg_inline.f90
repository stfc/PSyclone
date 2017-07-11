  MODULE psy_single_function
    USE lfric
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_testkern_type(f1, f2, m1)
      TYPE(field_type), intent(inout) :: f1, f2, m1
      INTEGER nlayers, ndf
      INTEGER, pointer :: map(:)
      INTEGER cell
      nlayers = f1%get_nlayers()
      ndf = f1%vspace%get_ndf()
      DO cell=1,f1%get_ncell()
        CALL f1%vspace%get_cell_dofmap(cell, map)
        CALL testkern_code(nlayers, ndf, map, f1%data, f2%data, m1%data)
      END DO 
    END SUBROUTINE invoke_testkern_type
    SUBROUTINE testkern_code(a, b, c)
    END SUBROUTINE testkern_code
  END MODULE psy_single_function
