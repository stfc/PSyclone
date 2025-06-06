!!=====================================================================================
!! ***  traadv kernel extracted from the NEMO software (http://www.nemo-ocean.eu ) ***
!! ***          governed by the CeCILL licence (http://www.cecill.info)            ***
!!
!! ***                             IS-ENES2 - CMCC/STFC                            ***
!!=====================================================================================
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2020-2025, Science and Technology Facilities
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
! Modified by A. R. Porter, STFC Daresbury Lab

PROGRAM tra_adv
   USE iso_c_binding, only: C_INT64_T
   INTEGER, PARAMETER :: wp = KIND(1.0d0)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) :: t3sn, t3ns, t3ew, t3we
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: tsn 
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: pun, pvn, pwn
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: mydomain, zslpx, zslpy, zwx, zwy, umask, vmask, tmask, zind
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)     :: ztfreez, rnfmsk, upsmsk
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:)       :: rnfmsk_z
   REAL(wp) :: zice, zu, z0u, zzwx, zv, z0v, zzwy, ztra, zbtr, zdt, zalpha
   REAL(wp) :: r
   REAL(wp) :: zw, z0w
   INTEGER  :: jpi, jpj, jpk, ji, jj, jk, jt
   ! TODO #588 it would be more natural to do INTEGER*8 here but PSyclone does
   ! not yet support such syntax.
   INTEGER(KIND=C_INT64_T) :: it
   CHARACTER(len=10) :: env

   CALL get_environment_variable("JPI", env)
   READ ( env, '(i10)' ) jpi
   CALL get_environment_variable("JPJ", env)
   READ ( env, '(i10)' ) jpj
   CALL get_environment_variable("JPK", env)
   READ ( env, '(i10)' ) jpk
   CALL get_environment_variable("IT", env)
   READ ( env, '(i10)' ) it

   IF(jpi < 1 .or. jpj < 1 .or. jpk < 1 .or. it < 1)THEN
      WRITE (*, "('Domain size (JPI * JPJ * JPK) and number of iterations (IT) " &
           //"must be set ',/'using environment variables, e.g.:',/'   "         &
           //"JPI=100 JPJ=100 JPK=30 IT=10 ./tra_adv.exe')")
      STOP
   END IF

   WRITE (*, "('Tracer-advection Mini-app:')")
   WRITE (*, "('Domain is ', I4, 'x', I4, ' grid points')") jpi, jpj
   WRITE (*, "('Performing ', I4, ' iterations')") it

   ! Initialisation

   ALLOCATE( mydomain (jpi,jpj,jpk))
   ALLOCATE( zwx (jpi,jpj,jpk))
   ALLOCATE( zwy (jpi,jpj,jpk))
   ALLOCATE( zslpx (jpi,jpj,jpk))
   ALLOCATE( zslpy (jpi,jpj,jpk))
   ALLOCATE( pun (jpi,jpj,jpk))
   ALLOCATE( pvn (jpi,jpj,jpk))
   ALLOCATE( pwn (jpi,jpj,jpk))
   ALLOCATE( umask (jpi,jpj,jpk))
   ALLOCATE( vmask (jpi,jpj,jpk))
   ALLOCATE( tmask (jpi,jpj,jpk))
   ALLOCATE( zind (jpi,jpj,jpk))
   ALLOCATE( ztfreez (jpi,jpj))
   ALLOCATE( rnfmsk (jpi,jpj))
   ALLOCATE( upsmsk (jpi,jpj))
   ALLOCATE( rnfmsk_z (jpk))
   ALLOCATE( tsn(jpi,jpj,jpk))

! arrays initialization

   r = jpi*jpj*jpk

   ! the following three lines can be uncommented to randomize arrays initialization
   !call random_seed()
   !call random_number(r)
   !r = r*jpi*jpj*jpk

   DO jk = 1, jpk
      DO jj = 1, jpj
          DO ji = 1, jpi
              umask(ji,jj,jk) = ji*jj*jk/r
              mydomain(ji,jj,jk) =ji*jj*jk/r
              pun(ji,jj,jk) =ji*jj*jk/r
              pvn(ji,jj,jk) =ji*jj*jk/r
              pwn(ji,jj,jk) =ji*jj*jk/r
              vmask(ji,jj,jk)= ji*jj*jk/r
              tsn(ji,jj,jk)= ji*jj*jk/r
              tmask(ji,jj,jk)= ji*jj*jk/r
          END DO
      END DO
   END DO

   r = jpi*jpj
   DO jj=1, jpj
      DO ji=1, jpi
         ztfreez(ji,jj) = ji*jj/r
         upsmsk(ji,jj) = ji*jj/r
         rnfmsk(ji,jj) = ji*jj/r
      END DO
   END DO

   DO jk=1, jpk
      rnfmsk_z(jk)=jk/jpk
   END DO

!***********************
!* Start of the symphony
!***********************

   DO jt = 1, it
      DO jk = 1, jpk
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( tsn(ji,jj,jk) <= ztfreez(ji,jj) + 0.1d0 ) THEN   ;   zice = 1.d0
               ELSE                                                 ;   zice = 0.d0
               ENDIF
               zind(ji,jj,jk) = MAX (   &
                   rnfmsk(ji,jj) * rnfmsk_z(jk),      & 
                   upsmsk(ji,jj)               ,      &
                   zice                               &
                   &                  ) * tmask(ji,jj,jk)
                   zind(ji,jj,jk) = 1 - zind(ji,jj,jk)
            END DO
         END DO
      END DO

      zwx(:,:,jpk) = 0.e0   ;   zwy(:,:,jpk) = 0.e0

      DO jk = 1, jpk-1
         DO jj = 1, jpj-1
            DO ji = 1, jpi-1
               zwx(ji,jj,jk) = umask(ji,jj,jk) * ( mydomain(ji+1,jj,jk) - mydomain(ji,jj,jk) )
               zwy(ji,jj,jk) = vmask(ji,jj,jk) * ( mydomain(ji,jj+1,jk) - mydomain(ji,jj,jk) )
            END DO
         END DO
      END DO

      zslpx(:,:,jpk) = 0.e0   ;   zslpy(:,:,jpk) = 0.e0 

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
         zdt  = 1
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
               ztra = - zbtr * ( zwx(ji,jj,jk) - zwx(ji-1,jj  ,jk  )   &
               &               + zwy(ji,jj,jk) - zwy(ji  ,jj-1,jk  ) )
               mydomain(ji,jj,jk) = mydomain(ji,jj,jk) + ztra
            END DO
         END DO
      END DO

      zwx (:,:, 1 ) = 0.e0    ;    zwx (:,:,jpk) = 0.e0

      DO jk = 2, jpk-1   
         zwx(:,:,jk) = tmask(:,:,jk) * ( mydomain(:,:,jk-1) - mydomain(:,:,jk) )
      END DO

      zslpx(:,:,1) = 0.e0

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

      zwx(:,:, 1 ) = pwn(:,:,1) * mydomain(:,:,1)

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
               ztra = - zbtr * ( zwx(ji,jj,jk) - zwx(ji,jj,jk+1) )
               mydomain(ji,jj,jk) = ztra
            END DO
         END DO
      END DO
   END DO

   OPEN(unit = 4, file = 'output.dat', form='formatted')
  
   DO jk = 1, jpk-1
      DO jj = 2, jpj-1
         DO ji = 2, jpi-1
            write(4,*) mydomain(ji,jj,jk)
         END DO
      END DO
   END DO

   CLOSE(4)

   DEALLOCATE( mydomain )
   DEALLOCATE( zwx )
   DEALLOCATE( zwy )
   DEALLOCATE( zslpx )
   DEALLOCATE( zslpy )
   DEALLOCATE( pun )
   DEALLOCATE( pvn )
   DEALLOCATE( pwn )
   DEALLOCATE( umask)
   DEALLOCATE( vmask)
   DEALLOCATE( tmask)
   DEALLOCATE( zind )
   DEALLOCATE( ztfreez )
   DEALLOCATE( rnfmsk)
   DEALLOCATE( upsmsk)
   DEALLOCATE( rnfmsk_z)
   DEALLOCATE( tsn)

   WRITE (*, "('Mini-app finished.')")

END PROGRAM tra_adv
