MODULE stringop
!$AGRIF_DO_NOT_TREAT
!-
!$Id: stringop.f90 2281 2010-10-15 14:21:13Z smasson $
!-
! This software is governed by the CeCILL license
! See IOIPSL/IOIPSL_License_CeCILL.txt
!---------------------------------------------------------------------
CONTAINS
!=
SUBROUTINE cmpblank (str)
!---------------------------------------------------------------------
!- Compact blanks
!---------------------------------------------------------------------
  CHARACTER(LEN=*),INTENT(inout) :: str
!-
  INTEGER :: lcc,ipb
!---------------------------------------------------------------------
  lcc = LEN_TRIM(str)
  ipb = 1
  DO
    IF (ipb >= lcc)   EXIT
    IF (str(ipb:ipb+1) == '  ') THEN
      str(ipb+1:) = str(ipb+2:lcc)
      lcc = lcc-1
    ELSE
      ipb = ipb+1
    ENDIF
  ENDDO
!----------------------
END SUBROUTINE cmpblank
!===
END MODULE stringop
