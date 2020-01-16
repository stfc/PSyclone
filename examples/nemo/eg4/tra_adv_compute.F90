module tra_adv_compute_mod

  implicit none

contains

  subroutine tra_adv_compute(pun, pvn, pwn, umask, vmask, tmask, mydomain, jpi, jpj, jpk)
    implicit none

    REAL*8, DIMENSION(:,:,:), intent(in)   :: pun, pvn, pwn, umask, vmask, tmask
    REAL*8, DIMENSION(:,:,:), intent(inout)   :: mydomain
    INTEGER, INTENT(IN) :: jpi, jpj, jpk
    ! Not sure what to do with 2D and 1D arrays in the SIR backend
    !REAL*8, DIMENSION(:,:)     :: ztfreez, rnfmsk, upsmsk
    !REAL*8, DIMENSION(:)       :: rnfmsk_z
    
    ! local variables
    REAL*8, DIMENSION(jpi,jpj, jpk)               :: zslpx, zslpy, zwx, zwy, zind
    REAL*8                                        :: zu, z0u, zzwx, zv, z0v, zzwy, ztra, zbtr, zdt, zalpha
    REAL*8                                        :: zw, z0w
    INTEGER                                       :: ji, jj, jk

    ! Not sure what to do with 2D and 1D arrays in the SIR backend
    !DO jk = 1, jpk
    !   DO jj = 1, jpj
    !      DO ji = 1, jpi
    !         IF( tsn(ji,jj,jk) <= ztfreez(ji,jj) + 0.1d0 ) THEN   ;   zice = 1.d0
    !         ELSE                                                 ;   zice = 0.d0
    !         ENDIF
    !         zind(ji,jj,jk) = MAX (   &
    !            rnfmsk(ji,jj) * rnfmsk_z(jk),      & 
    !            upsmsk(ji,jj)               ,      &
    !            zice                               &
    !            &                  ) * tmask(ji,jj,jk)
    !         zind(ji,jj,jk) = 1 - zind(ji,jj,jk)
    !      END DO
    !   END DO
    !END DO

    ! Not sure how to cope with implicit loops in the SIR backend
    ! zwx(:,:,jpk) = 0.e0   ;   zwy(:,:,jpk) = 0.e0

    DO jk = 1, jpk-1
       DO jj = 1, jpj-1
          DO ji = 1, jpi-1
             zwx(ji,jj,jk) = umask(ji,jj,jk) * ( mydomain(ji+1,jj,jk) - mydomain(ji,jj,jk) )
             zwy(ji,jj,jk) = vmask(ji,jj,jk) * ( mydomain(ji,jj+1,jk) - mydomain(ji,jj,jk) )
          END DO
       END DO
    END DO

    ! Not sure how to cope with implicit loops in the SIR backend
    ! zslpx(:,:,jpk) = 0.e0   ;   zslpy(:,:,jpk) = 0.e0 

    DO jk = 1, jpk-1
       DO jj = 2, jpj
          DO ji = 2, jpi 
             zslpx(ji,jj,jk) =                    ( zwx(ji,jj,jk) + zwx(ji-1,jj  ,jk) )   &
                  &            * ( 0.25d0 + SIGN( 0.25d0, zwx(ji,jj,jk) * zwx(ji-1,jj  ,jk) ) )
             zslpy(ji,jj,jk) =                    ( zwy(ji,jj,jk) + zwy(ji  ,jj-1,jk) )   &
                  &            * ( 0.25d0 + SIGN( 0.25d0, zwy(ji,jj,jk) * zwy(ji  ,jj-1,jk) ) )
          END DO
       END DO
    END DO

    DO jk = 1, jpk-1    
       DO jj = 2, jpj
          DO ji = 2, jpi
             zslpx(ji,jj,jk) = SIGN( 1.d0, zslpx(ji,jj,jk) ) * MIN(    ABS( zslpx(ji  ,jj,jk) ),   &
                  &                                                2.d0*ABS( zwx  (ji-1,jj,jk) ),   &
                  &                                                2.d0*ABS( zwx  (ji  ,jj,jk) ) )
             zslpy(ji,jj,jk) = SIGN( 1.d0, zslpy(ji,jj,jk) ) * MIN(    ABS( zslpy(ji,jj  ,jk) ),   &
                  &                                                2.d0*ABS( zwy  (ji,jj-1,jk) ),   &
                  &                                                2.d0*ABS( zwy  (ji,jj  ,jk) ) )
          END DO
       END DO
    END DO

    DO jk = 1, jpk-1
       ! SIR backend only supports perfectly nested triple loops
       ! zdt  = 1
       DO jj = 2, jpj-1
          DO ji = 2, jpi-1
             z0u = SIGN( 0.5d0, pun(ji,jj,jk) )
             zalpha = 0.5d0 - z0u
             zu  = z0u - 0.5d0 * pun(ji,jj,jk) * zdt
             
             zzwx = mydomain(ji+1,jj,jk) + zind(ji,jj,jk) * (zu * zslpx(ji+1,jj,jk))
             zzwy = mydomain(ji  ,jj,jk) + zind(ji,jj,jk) * (zu * zslpx(ji  ,jj,jk))
             
             zwx(ji,jj,jk) = pun(ji,jj,jk) * ( zalpha * zzwx + (1.-zalpha) * zzwy )
             
             z0v = SIGN( 0.5d0, pvn(ji,jj,jk) )
             zalpha = 0.5d0 - z0v
             zv  = z0v - 0.5d0 * pvn(ji,jj,jk) * zdt
             
             zzwx = mydomain(ji,jj+1,jk) + zind(ji,jj,jk) * (zv * zslpy(ji,jj+1,jk))
             zzwy = mydomain(ji,jj  ,jk) + zind(ji,jj,jk) * (zv * zslpy(ji,jj  ,jk))
             
             zwy(ji,jj,jk) = pvn(ji,jj,jk) * ( zalpha * zzwx + (1.d0-zalpha) * zzwy )
          END DO
       END DO
    END DO
    
    DO jk = 1, jpk-1
       DO jj = 2, jpj-1     
          DO ji = 2, jpi-1
             zbtr = 1.
             ! SIR backend does not like "-"
             !ztra = - zbtr * ( zwx(ji,jj,jk) - zwx(ji-1,jj  ,jk  )   &
             !     &               + zwy(ji,jj,jk) - zwy(ji  ,jj-1,jk  ) )
             ztra = zbtr * ( zwx(ji,jj,jk) - zwx(ji-1,jj  ,jk  )   &
                  &               + zwy(ji,jj,jk) - zwy(ji  ,jj-1,jk  ) )
             mydomain(ji,jj,jk) = mydomain(ji,jj,jk) + ztra
          END DO
       END DO
    END DO
    
    ! Not sure how to cope with implicit loops in the SIR backend
    ! zwx (:,:, 1 ) = 0.e0    ;    zwx (:,:,jpk) = 0.e0
    
    ! Not sure how to cope with implicit loops in the SIR backend
    ! DO jk = 2, jpk-1   
    !     zwx(:,:,jk) = tmask(:,:,jk) * ( mydomain(:,:,jk-1) - mydomain(:,:,jk) )
    ! END DO

    ! Not sure how to cope with implicit loops in the SIR backend
    ! zslpx(:,:,1) = 0.e0
    
    DO jk = 2, jpk-1    
       DO jj = 1, jpj
          DO ji = 1, jpi
             zslpx(ji,jj,jk) =                    ( zwx(ji,jj,jk) + zwx(ji,jj,jk+1) )   &
                  &            * ( 0.25d0 + SIGN( 0.25d0, zwx(ji,jj,jk) * zwx(ji,jj,jk+1) ) )
          END DO
       END DO
    END DO

    DO jk = 2, jpk-1     
       DO jj = 1, jpj
          DO ji = 1, jpi
             zslpx(ji,jj,jk) = SIGN( 1.d0, zslpx(ji,jj,jk) ) * MIN( ABS( zslpx(ji,jj,jk  ) ), &
                  &                                               2.d0*ABS( zwx  (ji,jj,jk+1) ),   &
                  &                                               2.d0*ABS( zwx  (ji,jj,jk  ) )  )
          END DO
       END DO
    END DO
    
    ! Not sure how to cope with implicit loops in the SIR backend
    ! zwx(:,:, 1 ) = pwn(:,:,1) * mydomain(:,:,1)

    zdt  = 1
    zbtr = 1.
    DO jk = 1, jpk-1
       DO jj = 2, jpj-1     
          DO ji = 2, jpi-1
             z0w = SIGN( 0.5d0, pwn(ji,jj,jk+1) )
             zalpha = 0.5d0 + z0w
             zw  = z0w - 0.5d0 * pwn(ji,jj,jk+1) * zdt * zbtr
             
             zzwx = mydomain(ji,jj,jk+1) + zind(ji,jj,jk) * (zw * zslpx(ji,jj,jk+1))
             zzwy = mydomain(ji,jj,jk  ) + zind(ji,jj,jk) * (zw * zslpx(ji,jj,jk  ))
             
             zwx(ji,jj,jk+1) = pwn(ji,jj,jk+1) * ( zalpha * zzwx + (1.-zalpha) * zzwy )
          END DO
       END DO
    END DO

    zbtr = 1.
    DO jk = 1, jpk-1
       DO jj = 2, jpj-1     
          DO ji = 2, jpi-1
             ! SIR backend does not like "-"
             ! ztra = - zbtr * ( zwx(ji,jj,jk) - zwx(ji,jj,jk+1) )
             ztra = zbtr * ( zwx(ji,jj,jk) - zwx(ji,jj,jk+1) )
             mydomain(ji,jj,jk) = ztra
          END DO
       END DO
    END DO

  end subroutine tra_adv_compute

end module tra_adv_compute_mod
