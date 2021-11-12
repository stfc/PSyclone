program tracer_advection
  ! USE dl_timer, only: timer_init, timer_register, timer_start, timer_stop, timer_report
  ! USE tra_adv_compute_mod, only: tra_adv_compute
  use res_cpp, only: run_nemo_from_host_cpp
  ! use res_cuda, only: run_nemo_from_host_cuda
  implicit none
  REAL*8, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: tsn 
  REAL*8, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: pun, pvn, pwn
  REAL*8, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: mydomain_cuda, mydomain_cpp, umask, vmask, tmask, zind
  REAL*8, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: zslpx
  REAL*8, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: ztfreez_dbg, rnfmsk_dbg, upsmsk_dbg
  REAL*8, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: rnfmsk_z_dbg
  REAL*8, ALLOCATABLE, SAVE, DIMENSION(:,:)     :: ztfreez, rnfmsk, upsmsk
  REAL*8, ALLOCATABLE, SAVE, DIMENSION(:)       :: rnfmsk_z
  REAL*8                                        :: r, checksum_cpp, checksum_cuda
  INTEGER*4                                     :: jpi, jpj, jpk, ji, jj, jk, jt
  INTEGER*4                                     :: itn_count
  CHARACTER(len=10)                             :: env
  !> Timer indexes, one for initialisation, one for the 'time-stepping'
  INTEGER :: init_timer, step_timer

  CALL get_environment_variable("JPI", env)
  READ ( env, '(i10)' ) jpi
  CALL get_environment_variable("JPJ", env)
  READ ( env, '(i10)' ) jpj
  CALL get_environment_variable("JPK", env)
  READ ( env, '(i10)' ) jpk
  CALL get_environment_variable("IT", env)
  READ ( env, '(i10)' ) itn_count

  ! Set-up our timers

  ! CALL timer_init()
  ! CALL timer_register(init_timer, label='Initialisation')
  ! CALL timer_register(step_timer, label='Time-stepping', num_repeats=itn_count)

  ! Initialisation

  ! call timer_start(init_timer)

  ALLOCATE( mydomain_cuda (jpi,jpj,jpk), &
            mydomain_cpp (jpi,jpj,jpk), &
            pun (jpi,jpj,jpk), &
            pvn (jpi,jpj,jpk), &
            pwn (jpi,jpj,jpk), &
            umask (jpi,jpj,jpk), &
            vmask (jpi,jpj,jpk), &
            tmask (jpi,jpj,jpk), &
            zind (jpi,jpj,jpk), &
            ztfreez (jpi,jpj), &
            rnfmsk (jpi,jpj), &
            upsmsk (jpi,jpj), &
            rnfmsk_z (jpk), &
            tsn(jpi,jpj,jpk), &
            zslpx(jpi,jpj,jpk))

   ALLOCATE( ztfreez_dbg (jpi,jpj,jpk), &
         rnfmsk_dbg (jpi,jpj,jpk), &
         upsmsk_dbg (jpi,jpj,jpk), &
         rnfmsk_z_dbg (jpi,jpj,jpk))         

  ! Array initialization

  r = jpi*jpj*jpk

  ! the following three lines can be uncommented to randomize arrays initialization
  !call random_seed()
  !call random_number(r)
  !r = r*jpi*jpj*jpk

  DO jk = 1, jpk
     DO jj = 1, jpj
        DO ji = 1, jpi
           umask(ji,jj,jk) = ji*jj*jk/r
           mydomain_cuda(ji,jj,jk) =ji*jj*jk/r
           mydomain_cpp(ji,jj,jk) =ji*jj*jk/r
           pun(ji,jj,jk) =ji*jj*jk/r
           pvn(ji,jj,jk) =ji*jj*jk/r
           pwn(ji,jj,jk) =ji*jj*jk/r
           vmask(ji,jj,jk)= ji*jj*jk/r
           tsn(ji,jj,jk)= ji*jj*jk/r
           tmask(ji,jj,jk)= ji*jj*jk/r

           ztfreez_dbg(ji,jj,jk)= ji*jj*jk/r
           rnfmsk_dbg(ji,jj,jk)= ji*jj*jk/r
           upsmsk_dbg(ji,jj,jk)= ji*jj*jk/r
           rnfmsk_z_dbg(ji,jj,jk)= ji*jj*jk/r
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

  ! call timer_stop(init_timer)

  ! call timer_start(step_timer)

  do jt = 1, itn_count
        print *, jt
        call run_nemo_from_host_cpp(jpi, jpj, jpk, ztfreez_dbg, pwn, vmask, rnfmsk_dbg, mydomain_cpp, tmask, umask, tsn, & 
                pvn, rnfmsk_z_dbg, pun, upsmsk_dbg, zslpx)       
        !call run_nemo_from_host_cuda(jpi, jpj, jpk, ztfreez_dbg, pwn, vmask, rnfmsk_dbg, mydomain_cuda, tmask, umask, tsn, & 
        !        pvn, rnfmsk_z_dbg, pun, upsmsk_dbg, zslpx)

  end do

  ! call timer_stop(step_timer)

  ! Output final field and compute checksum

  open(unit = 24, file = 'output.dat', form='formatted')

  checksum_cpp = 0.0d0
  checksum_cuda = 0.0d0
  do jk = 1, jpk-1
     do jj = 2, jpj-1
        do ji = 2, jpi-1
           checksum_cpp = checksum_cpp + mydomain_cpp(ji,jj,jk)
           checksum_cuda = checksum_cuda + mydomain_cuda(ji,jj,jk)
           write(24,*) mydomain_cpp(ji,jj,jk)
        end do
     end do
  end do

  write(*, "('Checksum for domain ', 2(I4, ' x'), I4, ' (',I4,' iterations) = ',E23.16 ,E23.16)") &
       jpi, jpj, jpk, itn_count, checksum_cpp, checksum_cuda

  close(24)

!   deallocate( mydomain_cpp )
!   deallocate( mydomain_cuda )
!   deallocate( pun )
!   deallocate( pvn )
!   deallocate( pwn )
!   deallocate( umask)
!   deallocate( vmask)
!   deallocate( tmask)
!   deallocate( zind )
!   deallocate( ztfreez )
!   deallocate( rnfmsk)
!   deallocate( upsmsk)
!   deallocate( rnfmsk_z)
!   deallocate( tsn)

  ! call timer_report()

end program tracer_advection
