! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2017-2020, Science and Technology Facilities
! Council.
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Original code distributed with the following license:
!!----------------------------------------------------------------------
!! NEMO/OPA 3.7 , NEMO Consortium (2015)
!! $Id: traldf_iso.F90 9124 2017-12-19 08:26:25Z gm $
!! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
!!----------------------------------------------------------------------
! Modifications: A. R. Porter, STFC Daresbury Lab
! Modified by R. W. Ford, STFC Daresbury Lab

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
        zmsku = 1. / MAX(wmask(ji + 1, jj, jk) + wmask(ji, jj, jk + 1) + wmask(ji + 1, jj, jk + 1) + wmask(ji, jj, jk), 1.)
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
