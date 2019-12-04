MODULE traldf_iso
   !!======================================================================
   !!                   ***  MODULE  traldf_iso  ***
   !! Ocean  tracers:  horizontal component of the lateral tracer mixing trend
   !!======================================================================
   !! History :  OPA  ! 1994-08  (G. Madec, M. Imbard)
   !!            8.0  ! 1997-05  (G. Madec)  split into traldf and trazdf
   !!            NEMO ! 2002-08  (G. Madec)  Free form, F90
   !!            1.0  ! 2005-11  (G. Madec)  merge traldf and trazdf :-)
   !!            3.3  ! 2010-09  (C. Ethe, G. Madec) Merge TRA-TRC
   !!            3.7  ! 2014-01  (G. Madec, S. Masson)  restructuration/simplification of aht/aeiv specification
   !!             -   ! 2014-02  (F. Lemarie, G. Madec)  triad operator (Griffies) + Method of Stabilizing Correction
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_ldf_iso   : update the tracer trend with the horizontal component of a iso-neutral laplacian operator
   !!                   and with the vertical part of the isopycnal or geopotential s-coord. operator 
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and active tracers
   USE dom_oce        ! ocean space and time domain
   USE trc_oce        ! share passive tracers/Ocean variables
   USE zdf_oce        ! ocean vertical physics
   USE ldftra         ! lateral diffusion: tracer eddy coefficients
   USE ldfslp         ! iso-neutral slopes
   USE diaptr         ! poleward transport diagnostics
   USE diaar5         ! AR5 diagnostics
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O library
   USE phycst         ! physical constants
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_ldf_iso   ! routine called by step.F90

   LOGICAL  ::   l_ptr   ! flag to compute poleward transport
   LOGICAL  ::   l_hst   ! flag to compute heat transport

   !! * Substitutions
!#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.7 , NEMO Consortium (2015)
   !! $Id: traldf_iso.F90 9124 2017-12-19 08:26:25Z gm $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

  SUBROUTINE tra_ldf_iso( kt, kit000, cdtype, pahu, pahv, pgu , pgv ,   &
      &                                                   pgui, pgvi,   &
      &                                       ptb , ptbb, pta , kjpt, kpass )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_ldf_iso  ***
      !!
      !! ** Purpose :   Compute the before horizontal tracer (t & s) diffusive 
      !!      trend for a laplacian tensor (ezxcept the dz[ dz[.] ] term) and 
      !!      add it to the general trend of tracer equation.
      !!
      !! ** Method  :   The horizontal component of the lateral diffusive trends 
      !!      is provided by a 2nd order operator rotated along neural or geopo-
      !!      tential surfaces to which an eddy induced advection can be added
      !!      It is computed using before fields (forward in time) and isopyc-
      !!      nal or geopotential slopes computed in routine ldfslp.
      !!
      !!      1st part :  masked horizontal derivative of T  ( di[ t ] )
      !!      ========    with partial cell update if ln_zps=T
      !!                  with top     cell update if ln_isfcav
      !!
      !!      2nd part :  horizontal fluxes of the lateral mixing operator
      !!      ========    
      !!         zftu =  pahu e2u*e3u/e1u di[ tb ]
      !!               - pahu e2u*uslp    dk[ mi(mk(tb)) ]
      !!         zftv =  pahv e1v*e3v/e2v dj[ tb ]
      !!               - pahv e2u*vslp    dk[ mj(mk(tb)) ]
      !!      take the horizontal divergence of the fluxes:
      !!         difft = 1/(e1e2t*e3t) {  di-1[ zftu ] +  dj-1[ zftv ]  }
      !!      Add this trend to the general trend (ta,sa):
      !!         ta = ta + difft
      !!
      !!      3rd part: vertical trends of the lateral mixing operator
      !!      ========  (excluding the vertical flux proportional to dk[t] )
      !!      vertical fluxes associated with the rotated lateral mixing:
      !!         zftw = - {  mi(mk(pahu)) * e2t*wslpi di[ mi(mk(tb)) ]
      !!                   + mj(mk(pahv)) * e1t*wslpj dj[ mj(mk(tb)) ]  }
      !!      take the horizontal divergence of the fluxes:
      !!         difft = 1/(e1e2t*e3t) dk[ zftw ]
      !!      Add this trend to the general trend (ta,sa):
      !!         pta = pta + difft
      !!
      !! ** Action :   Update pta arrays with the before rotated diffusion
      !!----------------------------------------------------------------------
      INTEGER                              , INTENT(in   ) ::   kt         ! ocean time-step index
      INTEGER                              , INTENT(in   ) ::   kit000     ! first time step index
      CHARACTER(len=3)                     , INTENT(in   ) ::   cdtype     ! =TRA or TRC (tracer indicator)
      INTEGER                              , INTENT(in   ) ::   kjpt       ! number of tracers
      INTEGER                              , INTENT(in   ) ::   kpass      ! =1/2 first or second passage
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   pahu, pahv ! eddy diffusivity at u- and v-points  [m2/s]
      REAL(wp), DIMENSION(jpi,jpj    ,kjpt), INTENT(in   ) ::   pgu, pgv   ! tracer gradient at pstep levels
      REAL(wp), DIMENSION(jpi,jpj,    kjpt), INTENT(in   ) ::   pgui, pgvi ! tracer gradient at top   levels
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt), INTENT(in   ) ::   ptb        ! tracer (kpass=1) or laplacian of tracer (kpass=2)
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt), INTENT(in   ) ::   ptbb       ! tracer (only used in kpass=2)
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt), INTENT(inout) ::   pta        ! tracer trend
      !
      INTEGER  ::  ji, jj, jk, jn   ! dummy loop indices
      INTEGER  ::  ikt
      INTEGER  ::  ierr             ! local integer
      REAL(wp) ::  zmsku, zahu_w, zabe1, zcof1, zcoef3   ! local scalars
      REAL(wp) ::  zmskv, zahv_w, zabe2, zcof2, zcoef4   !   -      -
      REAL(wp) ::  zcoef0, ze3w_2, zsign, z2dt, z1_2dt   !   -      -
      REAL(wp), DIMENSION(jpi,jpj)     ::   zdkt, zdk1t, z2d
      REAL(wp), DIMENSION(jpi,jpj,jpk) ::   zdit, zdjt, zftu, zftv, ztfw 
      !!----------------------------------------------------------------------
      !
      IF( kt == kit000 )  THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'tra_ldf_iso : rotated laplacian diffusion operator on ', cdtype
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
         !
         akz     (:,:,:) = 0._wp      
         ah_wslp2(:,:,:) = 0._wp
      ENDIF
      !   
      l_hst = .FALSE.
      l_ptr = .FALSE.
      IF( cdtype == 'TRA' .AND. ln_diaptr )                                                 l_ptr = .TRUE. 
      IF( cdtype == 'TRA' .AND. ( iom_use("uadv_heattr") .OR. iom_use("vadv_heattr") .OR. &
         &                        iom_use("uadv_salttr") .OR. iom_use("vadv_salttr")  ) )   l_hst = .TRUE.
      !
      !                                            ! set time step size (Euler/Leapfrog)
      IF( neuler == 0 .AND. kt == nit000 ) THEN   ;   z2dt =     rdt      ! at nit000   (Euler)
      ELSE                                        ;   z2dt = 2.* rdt      !             (Leapfrog)
      ENDIF
      z1_2dt = 1._wp / z2dt
      !
      IF( kpass == 1 ) THEN   ;   zsign =  1._wp      ! bilaplacian operator require a minus sign (eddy diffusivity >0)
      ELSE                    ;   zsign = -1._wp
      ENDIF
         
      !!----------------------------------------------------------------------
      !!   0 - calculate  ah_wslp2 and akz
      !!----------------------------------------------------------------------
      !
      IF( kpass == 1 ) THEN                  !==  first pass only  ==!
         !
         DO jk = 2, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  !
                  zmsku = wmask(ji,jj,jk) / MAX(   umask(ji  ,jj,jk-1) + umask(ji-1,jj,jk)          &
                     &                           + umask(ji-1,jj,jk-1) + umask(ji  ,jj,jk) , 1._wp  )
                  zmskv = wmask(ji,jj,jk) / MAX(   vmask(ji,jj  ,jk-1) + vmask(ji,jj-1,jk)          &
                     &                           + vmask(ji,jj-1,jk-1) + vmask(ji,jj  ,jk) , 1._wp  )
                     !
                  zahu_w = (   pahu(ji  ,jj,jk-1) + pahu(ji-1,jj,jk)    &
                     &       + pahu(ji-1,jj,jk-1) + pahu(ji  ,jj,jk)  ) * zmsku
                  zahv_w = (   pahv(ji,jj  ,jk-1) + pahv(ji,jj-1,jk)    &
                     &       + pahv(ji,jj-1,jk-1) + pahv(ji,jj  ,jk)  ) * zmskv
                     !
                  ah_wslp2(ji,jj,jk) = zahu_w * wslpi(ji,jj,jk) * wslpi(ji,jj,jk)   &
                     &               + zahv_w * wslpj(ji,jj,jk) * wslpj(ji,jj,jk)
               END DO
            END DO
         END DO
         !
         IF( ln_traldf_msc ) THEN                ! stabilizing vertical diffusivity coefficient
            DO jk = 2, jpkm1
               DO jj = 2, jpjm1
                  DO ji = fs_2, fs_jpim1
                     akz(ji,jj,jk) = 0.25_wp * (                                                                     &
                        &              ( pahu(ji  ,jj,jk) + pahu(ji  ,jj,jk-1) ) / ( e1u(ji  ,jj) * e1u(ji  ,jj) )   &
                        &            + ( pahu(ji-1,jj,jk) + pahu(ji-1,jj,jk-1) ) / ( e1u(ji-1,jj) * e1u(ji-1,jj) )   &
                        &            + ( pahv(ji,jj  ,jk) + pahv(ji,jj  ,jk-1) ) / ( e2v(ji,jj  ) * e2v(ji,jj  ) )   &
                        &            + ( pahv(ji,jj-1,jk) + pahv(ji,jj-1,jk-1) ) / ( e2v(ji,jj-1) * e2v(ji,jj-1) )   )
                  END DO
               END DO
            END DO
            !
            IF( ln_traldf_blp ) THEN                ! bilaplacian operator
               DO jk = 2, jpkm1
                  DO jj = 1, jpjm1
                     DO ji = 1, fs_jpim1
                        akz(ji,jj,jk) = 16._wp * ah_wslp2(ji,jj,jk)   &
                           &          * (  akz(ji,jj,jk) + ah_wslp2(ji,jj,jk) / ( e3w_n(ji,jj,jk) * e3w_n(ji,jj,jk) )  )
                     END DO
                  END DO
               END DO
            ELSEIF( ln_traldf_lap ) THEN              ! laplacian operator
               DO jk = 2, jpkm1
                  DO jj = 1, jpjm1
                     DO ji = 1, fs_jpim1
                        ze3w_2 = e3w_n(ji,jj,jk) * e3w_n(ji,jj,jk)
                        zcoef0 = z2dt * (  akz(ji,jj,jk) + ah_wslp2(ji,jj,jk) / ze3w_2  )
                        akz(ji,jj,jk) = MAX( zcoef0 - 0.5_wp , 0._wp ) * ze3w_2 * z1_2dt
                     END DO
                  END DO
               END DO
           ENDIF
           !
         ELSE                                    ! 33 flux set to zero with akz=ah_wslp2 ==>> computed in full implicit
            akz(:,:,:) = ah_wslp2(:,:,:)      
         ENDIF
      ENDIF
      !
      !                                                          ! ===========
      DO jn = 1, kjpt                                            ! tracer loop
         !                                                       ! ===========
         !                                               
         !!----------------------------------------------------------------------
         !!   I - masked horizontal derivative 
         !!----------------------------------------------------------------------
!!gm : bug.... why (x,:,:)?   (1,jpj,:) and (jpi,1,:) should be sufficient....
         zdit (1,:,:) = 0._wp     ;     zdit (jpi,:,:) = 0._wp
         zdjt (1,:,:) = 0._wp     ;     zdjt (jpi,:,:) = 0._wp
         !!end

         ! Horizontal tracer gradient 
         DO jk = 1, jpkm1
            DO jj = 1, jpjm1
               DO ji = 1, fs_jpim1   ! vector opt.
                  zdit(ji,jj,jk) = ( ptb(ji+1,jj  ,jk,jn) - ptb(ji,jj,jk,jn) ) * umask(ji,jj,jk)
                  zdjt(ji,jj,jk) = ( ptb(ji  ,jj+1,jk,jn) - ptb(ji,jj,jk,jn) ) * vmask(ji,jj,jk)
               END DO
            END DO
         END DO
         IF( ln_zps ) THEN      ! botton and surface ocean correction of the horizontal gradient
            DO jj = 1, jpjm1              ! bottom correction (partial bottom cell)
               DO ji = 1, fs_jpim1   ! vector opt.
                  zdit(ji,jj,mbku(ji,jj)) = pgu(ji,jj,jn)          
                  zdjt(ji,jj,mbkv(ji,jj)) = pgv(ji,jj,jn)
               END DO
            END DO
            IF( ln_isfcav ) THEN      ! first wet level beneath a cavity
               DO jj = 1, jpjm1
                  DO ji = 1, fs_jpim1   ! vector opt.
                     IF( miku(ji,jj) > 1 )   zdit(ji,jj,miku(ji,jj)) = pgui(ji,jj,jn)          
                     IF( mikv(ji,jj) > 1 )   zdjt(ji,jj,mikv(ji,jj)) = pgvi(ji,jj,jn)     
                  END DO
               END DO
            ENDIF
         ENDIF
         !
         !!----------------------------------------------------------------------
         !!   II - horizontal trend  (full)
         !!----------------------------------------------------------------------
         !
         DO jk = 1, jpkm1                                 ! Horizontal slab
            !
            !                             !== Vertical tracer gradient
            zdk1t(:,:) = ( ptb(:,:,jk,jn) - ptb(:,:,jk+1,jn) ) * wmask(:,:,jk+1)     ! level jk+1
            !
            IF( jk == 1 ) THEN   ;   zdkt(:,:) = zdk1t(:,:)                          ! surface: zdkt(jk=1)=zdkt(jk=2)
            ELSE                 ;   zdkt(:,:) = ( ptb(:,:,jk-1,jn) - ptb(:,:,jk,jn) ) * wmask(:,:,jk)
            ENDIF
            DO jj = 1 , jpjm1            !==  Horizontal fluxes
               DO ji = 1, fs_jpim1   ! vector opt.
                  zabe1 = pahu(ji,jj,jk) * e2_e1u(ji,jj) * e3u_n(ji,jj,jk)
                  zabe2 = pahv(ji,jj,jk) * e1_e2v(ji,jj) * e3v_n(ji,jj,jk)
                  !
                  zmsku = 1. / MAX(  wmask(ji+1,jj,jk  ) + wmask(ji,jj,jk+1)   &
                     &             + wmask(ji+1,jj,jk+1) + wmask(ji,jj,jk  ), 1. )
                  !
                  zmskv = 1. / MAX(  wmask(ji,jj+1,jk  ) + wmask(ji,jj,jk+1)   &
                     &             + wmask(ji,jj+1,jk+1) + wmask(ji,jj,jk  ), 1. )
                  !
                  zcof1 = - pahu(ji,jj,jk) * e2u(ji,jj) * uslp(ji,jj,jk) * zmsku
                  zcof2 = - pahv(ji,jj,jk) * e1v(ji,jj) * vslp(ji,jj,jk) * zmskv
                  !
                  zftu(ji,jj,jk ) = (  zabe1 * zdit(ji,jj,jk)   &
                     &               + zcof1 * (  zdkt (ji+1,jj) + zdk1t(ji,jj)      &
                     &                          + zdk1t(ji+1,jj) + zdkt (ji,jj)  )  ) * umask(ji,jj,jk)
                  zftv(ji,jj,jk) = (  zabe2 * zdjt(ji,jj,jk)   &
                     &               + zcof2 * (  zdkt (ji,jj+1) + zdk1t(ji,jj)      &
                     &                          + zdk1t(ji,jj+1) + zdkt (ji,jj)  )  ) * vmask(ji,jj,jk)                  
               END DO
            END DO
            !
            DO jj = 2 , jpjm1          !== horizontal divergence and add to pta
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  pta(ji,jj,jk,jn) = pta(ji,jj,jk,jn) + zsign * (  zftu(ji,jj,jk) - zftu(ji-1,jj,jk)      &
                     &                                           + zftv(ji,jj,jk) - zftv(ji,jj-1,jk)  )   &
                     &                                        * r1_e1e2t(ji,jj) / e3t_n(ji,jj,jk)
               END DO
            END DO
         END DO                                        !   End of slab  

         !!----------------------------------------------------------------------
         !!   III - vertical trend (full)
         !!----------------------------------------------------------------------
         !
         ztfw(1,:,:) = 0._wp     ;     ztfw(jpi,:,:) = 0._wp
         !
         ! Vertical fluxes
         ! ---------------
         !                          ! Surface and bottom vertical fluxes set to zero
         ztfw(:,:, 1 ) = 0._wp      ;      ztfw(:,:,jpk) = 0._wp
         
         DO jk = 2, jpkm1           ! interior (2=<jk=<jpk-1)
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  !
                  zmsku = wmask(ji,jj,jk) / MAX(   umask(ji  ,jj,jk-1) + umask(ji-1,jj,jk)          &
                     &                           + umask(ji-1,jj,jk-1) + umask(ji  ,jj,jk) , 1._wp  )
                  zmskv = wmask(ji,jj,jk) / MAX(   vmask(ji,jj  ,jk-1) + vmask(ji,jj-1,jk)          &
                     &                           + vmask(ji,jj-1,jk-1) + vmask(ji,jj  ,jk) , 1._wp  )
                     !
                  zahu_w = (   pahu(ji  ,jj,jk-1) + pahu(ji-1,jj,jk)    &
                     &       + pahu(ji-1,jj,jk-1) + pahu(ji  ,jj,jk)  ) * zmsku
                  zahv_w = (   pahv(ji,jj  ,jk-1) + pahv(ji,jj-1,jk)    &
                     &       + pahv(ji,jj-1,jk-1) + pahv(ji,jj  ,jk)  ) * zmskv
                     !
                  zcoef3 = - zahu_w * e2t(ji,jj) * zmsku * wslpi (ji,jj,jk)   !wslpi & j are already w-masked
                  zcoef4 = - zahv_w * e1t(ji,jj) * zmskv * wslpj (ji,jj,jk)
                  !
                  ztfw(ji,jj,jk) = zcoef3 * (   zdit(ji  ,jj  ,jk-1) + zdit(ji-1,jj  ,jk)      &
                     &                        + zdit(ji-1,jj  ,jk-1) + zdit(ji  ,jj  ,jk)  )   &
                     &           + zcoef4 * (   zdjt(ji  ,jj  ,jk-1) + zdjt(ji  ,jj-1,jk)      &
                     &                        + zdjt(ji  ,jj-1,jk-1) + zdjt(ji  ,jj  ,jk)  )
               END DO
            END DO
         END DO
         !                                !==  add the vertical 33 flux  ==!
         IF( ln_traldf_lap ) THEN               ! laplacian case: eddy coef = ah_wslp2 - akz
            DO jk = 2, jpkm1       
               DO jj = 1, jpjm1
                  DO ji = fs_2, fs_jpim1
                     ztfw(ji,jj,jk) = ztfw(ji,jj,jk) + e1e2t(ji,jj) / e3w_n(ji,jj,jk) * wmask(ji,jj,jk)   &
                        &                            * ( ah_wslp2(ji,jj,jk) - akz(ji,jj,jk) )             &
                        &                            * ( ptb(ji,jj,jk-1,jn) - ptb(ji,jj,jk,jn) )
                  END DO
               END DO
            END DO
            !
         ELSE                                   ! bilaplacian 
            SELECT CASE( kpass )
            CASE(  1  )                            ! 1st pass : eddy coef = ah_wslp2
               DO jk = 2, jpkm1 
                  DO jj = 1, jpjm1
                     DO ji = fs_2, fs_jpim1
                        ztfw(ji,jj,jk) = ztfw(ji,jj,jk)    &
                           &           + ah_wslp2(ji,jj,jk) * e1e2t(ji,jj)   &
                           &           * ( ptb(ji,jj,jk-1,jn) - ptb(ji,jj,jk,jn) ) / e3w_n(ji,jj,jk) * wmask(ji,jj,jk)
                     END DO
                  END DO
               END DO 
            CASE(  2  )                         ! 2nd pass : eddy flux = ah_wslp2 and akz applied on ptb  and ptbb gradients, resp.
               DO jk = 2, jpkm1 
                  DO jj = 1, jpjm1
                     DO ji = fs_2, fs_jpim1
                        ztfw(ji,jj,jk) = ztfw(ji,jj,jk) + e1e2t(ji,jj) / e3w_n(ji,jj,jk) * wmask(ji,jj,jk)                      &
                           &                            * (  ah_wslp2(ji,jj,jk) * ( ptb (ji,jj,jk-1,jn) - ptb (ji,jj,jk,jn) )   &
                           &                               + akz     (ji,jj,jk) * ( ptbb(ji,jj,jk-1,jn) - ptbb(ji,jj,jk,jn) )   )
                     END DO
                  END DO
               END DO
            END SELECT
         ENDIF
         !         
         DO jk = 1, jpkm1                 !==  Divergence of vertical fluxes added to pta  ==!
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  pta(ji,jj,jk,jn) = pta(ji,jj,jk,jn) + zsign * (  ztfw (ji,jj,jk) - ztfw(ji,jj,jk+1)  )   &
                     &                                        * r1_e1e2t(ji,jj) / e3t_n(ji,jj,jk)
               END DO
            END DO
         END DO
         !
         IF( ( kpass == 1 .AND. ln_traldf_lap ) .OR.  &     !==  first pass only (  laplacian)  ==!
             ( kpass == 2 .AND. ln_traldf_blp ) ) THEN      !==  2nd   pass      (bilaplacian)  ==!
            !
            !                             ! "Poleward" diffusive heat or salt transports (T-S case only)
               ! note sign is reversed to give down-gradient diffusive transports )
            IF( l_ptr )  CALL dia_ptr_hst( jn, 'ldf', -zftv(:,:,:)  )
            !                          ! Diffusive heat transports
            IF( l_hst )  CALL dia_ar5_hst( jn, 'ldf', -zftu(:,:,:), -zftv(:,:,:) )
            !
         ENDIF                                                    !== end pass selection  ==!
         !
         !                                                        ! ===============
      END DO                                                      ! end tracer loop
      !
   END SUBROUTINE tra_ldf_iso

   !!==============================================================================
END MODULE traldf_iso
