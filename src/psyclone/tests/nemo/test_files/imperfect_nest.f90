! Original code distributed with the following license:
!!----------------------------------------------------------------------
!! NEMO/OPA 3.7 , NEMO Consortium (2015)
!! $Id: traldf_iso.F90 9124 2017-12-19 08:26:25Z gm $
!! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
!!----------------------------------------------------------------------

program imperfect_nest
  USE dom_oce        ! ocean space and time domain
  implicit none
  integer :: ji, jj, jk, jn
  integer, parameter :: jpi=10, jpj=20, jpk=30, jpim1=9, jpjm1=19, jpkm1=29, kjpt=2
  real, dimension(jpi,jpj,jpk) :: umask, vmask, wmask, pahu, e3u_n, e3t_n, uslp
  REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt) :: ptb  ! tracer (kpass=1) or laplacian of tracer (kpass=2)
  REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt) :: ptbb ! tracer (only used in kpass=2)
  REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt) :: pta  ! tracer trend
  REAL(wp), DIMENSION(jpi,jpj)     ::   zdkt, zdk1t, e2_e1u, e2u, r1_e1e2t
  REAL(wp), DIMENSION(jpi,jpj,jpk) ::   zdit, zdjt, zftu, zftv, ztfw 
  REAL(wp) ::  zmsku, zahu_w, zabe1, zcof1, zcoef3, zsign   ! local scalars

  ! Test code with imperfectly nested loops
  DO jk = 1, jpkm1
    DO jj = 1, jpj, 1
      DO ji = 1, jpi, 1
        zdk1t(ji, jj) = (ptb(ji, jj, jk, jn) - ptb(ji, jj, jk + 1, jn)) * wmask(ji, jj, jk + 1)
      END DO
    END DO
    IF (jk == 1) THEN
       zdkt(:, :) = zdk1t(:, :)
    else if (jk == jpkm1) then
       zdkt(:, :) = 0.5*zdk1t(:, :)
    ELSE
       do jj = 1, jpj, 1
          do ji = 1, jpi, 1
             zdkt(ji, jj) = (ptb(ji, jj, jk - 1, jn) - ptb(ji, jj, jk, jn)) * wmask(ji, jj, jk)
          end do
       end do
    END IF
    DO jj = 1, jpjm1
      DO ji = 1, jpim1
        zabe1 = pahu(ji, jj, jk) * e2_e1u(ji, jj) * e3u_n(ji, jj, jk)
        zmsku = 1._wp / MAX(wmask(ji + 1, jj, jk) + wmask(ji, jj, jk + 1) + wmask(ji + 1, jj, jk + 1) + wmask(ji, jj, jk), 1.)
        zcof1 = - pahu(ji, jj, jk) * e2u(ji, jj) * uslp(ji, jj, jk) * zmsku
        zftu(ji, jj, jk) = (zabe1 * zdit(ji, jj, jk) + zcof1 * (zdkt(ji + 1, jj) + zdk1t(ji, jj) + zdk1t(ji + 1, jj) + zdkt(ji, jj))) * umask(ji, jj, jk)
      END DO
    END DO
    DO jj = 2, jpjm1
      DO ji = 2, jpim1
        pta(ji, jj, jk, jn) = pta(ji, jj, jk, jn) + zsign * (zftu(ji, jj, jk) - zftu(ji - 1, jj, jk) + zftv(ji, jj, jk) - zftv(ji, jj - 1, jk)) * r1_e1e2t(ji, jj) / e3t_n(ji, jj, jk)
      END DO
    END DO
  END DO
 
end program imperfect_nest
