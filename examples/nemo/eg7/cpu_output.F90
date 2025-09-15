program tra_adv
  use iso_c_binding, only : c_int64_t
  integer, parameter :: wp = 8
  real(kind=wp), allocatable, dimension(:,:,:,:), save :: t3sn
  real(kind=wp), allocatable, dimension(:,:,:,:), save :: t3ns
  real(kind=wp), allocatable, dimension(:,:,:,:), save :: t3ew
  real(kind=wp), allocatable, dimension(:,:,:,:), save :: t3we
  real(kind=wp), allocatable, dimension(:,:,:), save :: tsn
  real(kind=wp), allocatable, dimension(:,:,:), save :: pun
  real(kind=wp), allocatable, dimension(:,:,:), save :: pvn
  real(kind=wp), allocatable, dimension(:,:,:), save :: pwn
  real(kind=wp), allocatable, dimension(:,:,:), save :: mydomain
  real(kind=wp), allocatable, dimension(:,:,:), save :: zslpx
  real(kind=wp), allocatable, dimension(:,:,:), save :: zslpy
  real(kind=wp), allocatable, dimension(:,:,:), save :: zwx
  real(kind=wp), allocatable, dimension(:,:,:), save :: zwy
  real(kind=wp), allocatable, dimension(:,:,:), save :: umask
  real(kind=wp), allocatable, dimension(:,:,:), save :: vmask
  real(kind=wp), allocatable, dimension(:,:,:), save :: tmask
  real(kind=wp), allocatable, dimension(:,:,:), save :: zind
  real(kind=wp), allocatable, dimension(:,:), save :: ztfreez
  real(kind=wp), allocatable, dimension(:,:), save :: rnfmsk
  real(kind=wp), allocatable, dimension(:,:), save :: upsmsk
  real(kind=wp), allocatable, dimension(:), save :: rnfmsk_z
  real(kind=wp) :: zice
  real(kind=wp) :: zu
  real(kind=wp) :: z0u
  real(kind=wp) :: zzwx
  real(kind=wp) :: zv
  real(kind=wp) :: z0v
  real(kind=wp) :: zzwy
  real(kind=wp) :: ztra
  real(kind=wp) :: zbtr
  real(kind=wp) :: zdt
  real(kind=wp) :: zalpha
  real(kind=wp) :: r
  real(kind=wp) :: zw
  real(kind=wp) :: z0w
  integer :: jpi
  integer :: jpj
  integer :: jpk
  integer :: ji
  integer :: jj
  integer :: jk
  integer :: jt
  integer(kind=c_int64_t) :: it
  CHARACTER(LEN = 10) :: env
  integer :: idx
  integer :: idx_1
  integer :: idx_2
  integer :: idx_3
  integer :: idx_4
  integer :: idx_5
  integer :: idx_6
  integer :: idx_7
  integer :: idx_8
  integer :: idx_9
  integer :: idx_10
  integer :: idx_11
  integer :: idx_12
  integer :: idx_13
  integer :: idx_14
  integer :: idx_15
  integer :: idx_16
  integer :: idx_17

  call get_environment_variable('JPI', env)

  ! PSyclone CodeBlock (unsupported code) reason:
  !  - Unsupported statement: Read_Stmt
  READ(env, '(i10)') jpi
  call get_environment_variable('JPJ', env)

  ! PSyclone CodeBlock (unsupported code) reason:
  !  - Unsupported statement: Read_Stmt
  READ(env, '(i10)') jpj
  call get_environment_variable('JPK', env)

  ! PSyclone CodeBlock (unsupported code) reason:
  !  - Unsupported statement: Read_Stmt
  READ(env, '(i10)') jpk
  call get_environment_variable('IT', env)

  ! PSyclone CodeBlock (unsupported code) reason:
  !  - Unsupported statement: Read_Stmt
  READ(env, '(i10)') it
  ALLOCATE(mydomain(1:jpi,1:jpj,1:jpk))
  ALLOCATE(zwx(1:jpi,1:jpj,1:jpk))
  ALLOCATE(zwy(1:jpi,1:jpj,1:jpk))
  ALLOCATE(zslpx(1:jpi,1:jpj,1:jpk))
  ALLOCATE(zslpy(1:jpi,1:jpj,1:jpk))
  ALLOCATE(pun(1:jpi,1:jpj,1:jpk))
  ALLOCATE(pvn(1:jpi,1:jpj,1:jpk))
  ALLOCATE(pwn(1:jpi,1:jpj,1:jpk))
  ALLOCATE(umask(1:jpi,1:jpj,1:jpk))
  ALLOCATE(vmask(1:jpi,1:jpj,1:jpk))
  ALLOCATE(tmask(1:jpi,1:jpj,1:jpk))
  ALLOCATE(zind(1:jpi,1:jpj,1:jpk))
  ALLOCATE(ztfreez(1:jpi,1:jpj))
  ALLOCATE(rnfmsk(1:jpi,1:jpj))
  ALLOCATE(upsmsk(1:jpi,1:jpj))
  ALLOCATE(rnfmsk_z(1:jpk))
  ALLOCATE(tsn(1:jpi,1:jpj,1:jpk))
  r = jpi * jpj * jpk
  !$omp parallel default(shared) private(ji,jj,jk)
  !$omp do schedule(auto)
  do jk = 1, jpk, 1
    do jj = 1, jpj, 1
      do ji = 1, jpi, 1
        umask(ji,jj,jk) = ji * jj * jk / r
        mydomain(ji,jj,jk) = ji * jj * jk / r
        pun(ji,jj,jk) = ji * jj * jk / r
        pvn(ji,jj,jk) = ji * jj * jk / r
        pwn(ji,jj,jk) = ji * jj * jk / r
        vmask(ji,jj,jk) = ji * jj * jk / r
        tsn(ji,jj,jk) = ji * jj * jk / r
        tmask(ji,jj,jk) = ji * jj * jk / r
      enddo
    enddo
  enddo
  !$omp end do nowait
  !$omp barrier
  !$omp end parallel
  r = jpi * jpj
  !$omp parallel default(shared) private(ji,jj,jk)
  !$omp do schedule(auto)
  do jj = 1, jpj, 1
    do ji = 1, jpi, 1
      ztfreez(ji,jj) = ji * jj / r
      upsmsk(ji,jj) = ji * jj / r
      rnfmsk(ji,jj) = ji * jj / r
    enddo
  enddo
  !$omp end do nowait
  !$omp do schedule(auto)
  do jk = 1, jpk, 1
    rnfmsk_z(jk) = jk / jpk
  enddo
  !$omp end do nowait
  !$omp barrier
  !$omp end parallel
  do jt = 1, it, 1
    !$omp parallel default(shared)  &
!$omp& private(idx,idx_1,idx_10,idx_11,idx_12,idx_13,idx_14,idx_15,idx_16,idx_17,idx_2,idx_3,idx_4,idx_5,idx_6,idx_7,idx_8,idx_9, &
!$omp& ji,jj,jk,z0u,z0v,zalpha,zbtr,zdt,ztra,zu,zv,zzwx,zzwy) firstprivate(zice)
    !$omp do schedule(auto)
    do jk = 1, jpk, 1
      do jj = 1, jpj, 1
        do ji = 1, jpi, 1
          if (tsn(ji,jj,jk) <= ztfreez(ji,jj) + 0.1d0) then
            zice = 1.d0
          else
            zice = 0.d0
          end if
          zind(ji,jj,jk) = MAX(rnfmsk(ji,jj) * rnfmsk_z(jk), upsmsk(ji,jj), zice) * tmask(ji,jj,jk)
          zind(ji,jj,jk) = 1 - zind(ji,jj,jk)
        enddo
      enddo
    enddo
    !$omp end do nowait
    !$omp barrier
    !$omp do schedule(auto)
    do idx = LBOUND(zwx, dim=2), UBOUND(zwx, dim=2), 1
      do idx_1 = LBOUND(zwx, dim=1), UBOUND(zwx, dim=1), 1
        zwx(idx_1,idx,jpk) = 0.e0
      enddo
    enddo
    !$omp end do nowait
    !$omp do schedule(auto)
    do idx_2 = LBOUND(zwy, dim=2), UBOUND(zwy, dim=2), 1
      do idx_3 = LBOUND(zwy, dim=1), UBOUND(zwy, dim=1), 1
        zwy(idx_3,idx_2,jpk) = 0.e0
      enddo
    enddo
    !$omp end do nowait
    !$omp barrier
    !$omp do schedule(auto)
    do jk = 1, jpk - 1, 1
      do jj = 1, jpj - 1, 1
        do ji = 1, jpi - 1, 1
          zwx(ji,jj,jk) = umask(ji,jj,jk) * (mydomain(ji + 1,jj,jk) - mydomain(ji,jj,jk))
          zwy(ji,jj,jk) = vmask(ji,jj,jk) * (mydomain(ji,jj + 1,jk) - mydomain(ji,jj,jk))
        enddo
      enddo
    enddo
    !$omp end do nowait
    !$omp do schedule(auto)
    do idx_4 = LBOUND(zslpx, dim=2), UBOUND(zslpx, dim=2), 1
      do idx_5 = LBOUND(zslpx, dim=1), UBOUND(zslpx, dim=1), 1
        zslpx(idx_5,idx_4,jpk) = 0.e0
      enddo
    enddo
    !$omp end do nowait
    !$omp do schedule(auto)
    do idx_6 = LBOUND(zslpy, dim=2), UBOUND(zslpy, dim=2), 1
      do idx_7 = LBOUND(zslpy, dim=1), UBOUND(zslpy, dim=1), 1
        zslpy(idx_7,idx_6,jpk) = 0.e0
      enddo
    enddo
    !$omp end do nowait
    !$omp barrier
    !$omp do schedule(auto)
    do jk = 1, jpk - 1, 1
      do jj = 2, jpj, 1
        do ji = 2, jpi, 1
          zslpx(ji,jj,jk) = (zwx(ji,jj,jk) + zwx(ji - 1,jj,jk)) * (0.25d0 + SIGN(0.25d0, zwx(ji,jj,jk) * zwx(ji - 1,jj,jk)))
          zslpy(ji,jj,jk) = (zwy(ji,jj,jk) + zwy(ji,jj - 1,jk)) * (0.25d0 + SIGN(0.25d0, zwy(ji,jj,jk) * zwy(ji,jj - 1,jk)))
        enddo
      enddo
    enddo
    !$omp end do nowait
    !$omp barrier
    !$omp do schedule(auto)
    do jk = 1, jpk - 1, 1
      do jj = 2, jpj, 1
        do ji = 2, jpi, 1
          zslpx(ji,jj,jk) = SIGN(1.d0, zslpx(ji,jj,jk)) * MIN(ABS(zslpx(ji,jj,jk)), 2.d0 * ABS(zwx(ji - 1,jj,jk)), 2.d0 * &
&ABS(zwx(ji,jj,jk)))
          zslpy(ji,jj,jk) = SIGN(1.d0, zslpy(ji,jj,jk)) * MIN(ABS(zslpy(ji,jj,jk)), 2.d0 * ABS(zwy(ji,jj - 1,jk)), 2.d0 * &
&ABS(zwy(ji,jj,jk)))
        enddo
      enddo
    enddo
    !$omp end do nowait
    !$omp barrier
    !$omp do schedule(auto)
    do jk = 1, jpk - 1, 1
      zdt = 1
      do jj = 2, jpj - 1, 1
        do ji = 2, jpi - 1, 1
          z0u = SIGN(0.5d0, pun(ji,jj,jk))
          zalpha = 0.5d0 - z0u
          zu = z0u - 0.5d0 * pun(ji,jj,jk) * zdt
          zzwx = mydomain(ji + 1,jj,jk) + zind(ji,jj,jk) * (zu * zslpx(ji + 1,jj,jk))
          zzwy = mydomain(ji,jj,jk) + zind(ji,jj,jk) * (zu * zslpx(ji,jj,jk))
          zwx(ji,jj,jk) = pun(ji,jj,jk) * (zalpha * zzwx + (1. - zalpha) * zzwy)
          z0v = SIGN(0.5d0, pvn(ji,jj,jk))
          zalpha = 0.5d0 - z0v
          zv = z0v - 0.5d0 * pvn(ji,jj,jk) * zdt
          zzwx = mydomain(ji,jj + 1,jk) + zind(ji,jj,jk) * (zv * zslpy(ji,jj + 1,jk))
          zzwy = mydomain(ji,jj,jk) + zind(ji,jj,jk) * (zv * zslpy(ji,jj,jk))
          zwy(ji,jj,jk) = pvn(ji,jj,jk) * (zalpha * zzwx + (1.d0 - zalpha) * zzwy)
        enddo
      enddo
    enddo
    !$omp end do nowait
    !$omp barrier
    !$omp do schedule(auto)
    do jk = 1, jpk - 1, 1
      do jj = 2, jpj - 1, 1
        do ji = 2, jpi - 1, 1
          zbtr = 1.
          ztra = -zbtr * (zwx(ji,jj,jk) - zwx(ji - 1,jj,jk) + zwy(ji,jj,jk) - zwy(ji,jj - 1,jk))
          mydomain(ji,jj,jk) = mydomain(ji,jj,jk) + ztra
        enddo
      enddo
    enddo
    !$omp end do nowait
    !$omp barrier
    !$omp do schedule(auto)
    do idx_8 = LBOUND(zwx, dim=2), UBOUND(zwx, dim=2), 1
      do idx_9 = LBOUND(zwx, dim=1), UBOUND(zwx, dim=1), 1
        zwx(idx_9,idx_8,1) = 0.e0
      enddo
    enddo
    !$omp end do nowait
    !$omp barrier
    !$omp do schedule(auto)
    do idx_10 = LBOUND(zwx, dim=2), UBOUND(zwx, dim=2), 1
      do idx_11 = LBOUND(zwx, dim=1), UBOUND(zwx, dim=1), 1
        zwx(idx_11,idx_10,jpk) = 0.e0
      enddo
    enddo
    !$omp end do nowait
    !$omp barrier
    !$omp do schedule(auto)
    do jk = 2, jpk - 1, 1
      do idx_12 = LBOUND(zwx, dim=2), UBOUND(zwx, dim=2), 1
        do idx_13 = LBOUND(zwx, dim=1), UBOUND(zwx, dim=1), 1
          zwx(idx_13,idx_12,jk) = tmask(idx_13 + (LBOUND(tmask, dim=1) - LBOUND(zwx, dim=1)),idx_12 + (LBOUND(tmask, dim=2) - &
&LBOUND(zwx, dim=2)),jk) * (mydomain(idx_13 + (LBOUND(mydomain, dim=1) - LBOUND(zwx, dim=1)),idx_12 + (LBOUND(mydomain, dim=2) - &
&LBOUND(zwx, dim=2)),jk - 1) - mydomain(idx_13 + (LBOUND(mydomain, dim=1) - LBOUND(zwx, dim=1)),idx_12 + (LBOUND(mydomain, dim=2) &
&- LBOUND(zwx, dim=2)),jk))
        enddo
      enddo
    enddo
    !$omp end do nowait
    !$omp do schedule(auto)
    do idx_14 = LBOUND(zslpx, dim=2), UBOUND(zslpx, dim=2), 1
      do idx_15 = LBOUND(zslpx, dim=1), UBOUND(zslpx, dim=1), 1
        zslpx(idx_15,idx_14,1) = 0.e0
      enddo
    enddo
    !$omp end do nowait
    !$omp barrier
    !$omp do schedule(auto)
    do jk = 2, jpk - 1, 1
      do jj = 1, jpj, 1
        do ji = 1, jpi, 1
          zslpx(ji,jj,jk) = (zwx(ji,jj,jk) + zwx(ji,jj,jk + 1)) * (0.25d0 + SIGN(0.25d0, zwx(ji,jj,jk) * zwx(ji,jj,jk + 1)))
        enddo
      enddo
    enddo
    !$omp end do nowait
    !$omp barrier
    !$omp do schedule(auto)
    do jk = 2, jpk - 1, 1
      do jj = 1, jpj, 1
        do ji = 1, jpi, 1
          zslpx(ji,jj,jk) = SIGN(1.d0, zslpx(ji,jj,jk)) * MIN(ABS(zslpx(ji,jj,jk)), 2.d0 * ABS(zwx(ji,jj,jk + 1)), 2.d0 * &
&ABS(zwx(ji,jj,jk)))
        enddo
      enddo
    enddo
    !$omp end do nowait
    !$omp barrier
    !$omp do schedule(auto)
    do idx_16 = LBOUND(zwx, dim=2), UBOUND(zwx, dim=2), 1
      do idx_17 = LBOUND(zwx, dim=1), UBOUND(zwx, dim=1), 1
        zwx(idx_17,idx_16,1) = pwn(idx_17 + (LBOUND(pwn, dim=1) - LBOUND(zwx, dim=1)),idx_16 + (LBOUND(pwn, dim=2) - LBOUND(zwx, &
&dim=2)),1) * mydomain(idx_17 + (LBOUND(mydomain, dim=1) - LBOUND(zwx, dim=1)),idx_16 + (LBOUND(mydomain, dim=2) - LBOUND(zwx, &
&dim=2)),1)
      enddo
    enddo
    !$omp end do nowait
    !$omp end parallel
    zdt = 1
    !$omp parallel default(shared)
    !$omp barrier
    !$omp end parallel
    zbtr = 1.
    !$omp parallel default(shared) private(ji,jj,jk,z0w,zalpha,zw,zzwx,zzwy)
    !$omp do schedule(auto)
    do jk = 1, jpk - 1, 1
      do jj = 2, jpj - 1, 1
        do ji = 2, jpi - 1, 1
          z0w = SIGN(0.5d0, pwn(ji,jj,jk + 1))
          zalpha = 0.5d0 + z0w
          zw = z0w - 0.5d0 * pwn(ji,jj,jk + 1) * zdt * zbtr
          zzwx = mydomain(ji,jj,jk + 1) + zind(ji,jj,jk) * (zw * zslpx(ji,jj,jk + 1))
          zzwy = mydomain(ji,jj,jk) + zind(ji,jj,jk) * (zw * zslpx(ji,jj,jk))
          zwx(ji,jj,jk + 1) = pwn(ji,jj,jk + 1) * (zalpha * zzwx + (1. - zalpha) * zzwy)
        enddo
      enddo
    enddo
    !$omp end do nowait
    !$omp barrier
    !$omp end parallel
    zbtr = 1.
    !$omp parallel default(shared) private(ji,jj,jk,ztra)
    !$omp do schedule(auto)
    do jk = 1, jpk - 1, 1
      do jj = 2, jpj - 1, 1
        do ji = 2, jpi - 1, 1
          ztra = -zbtr * (zwx(ji,jj,jk) - zwx(ji,jj,jk + 1))
          mydomain(ji,jj,jk) = ztra
        enddo
      enddo
    enddo
    !$omp end do nowait
    !$omp end parallel
  enddo

  ! PSyclone CodeBlock (unsupported code) reason:
  !  - Unsupported statement: Open_Stmt
  OPEN(UNIT = 4, FILE = 'output.dat', FORM = 'formatted')
  !$omp parallel default(shared)
  !$omp barrier
  !$omp end parallel
  do jk = 1, jpk - 1, 1
    do jj = 2, jpj - 1, 1
      do ji = 2, jpi - 1, 1
        ! PSyclone CodeBlock (unsupported code) reason:
        !  - Unsupported statement: Write_Stmt
        WRITE(4, *) mydomain(ji, jj, jk)
      enddo
    enddo
  enddo

  ! PSyclone CodeBlock (unsupported code) reason:
  !  - Unsupported statement: Close_Stmt
  CLOSE(UNIT = 4)
  DEALLOCATE(mydomain)
  DEALLOCATE(zwx)
  DEALLOCATE(zwy)
  DEALLOCATE(zslpx)
  DEALLOCATE(zslpy)
  DEALLOCATE(pun)
  DEALLOCATE(pvn)
  DEALLOCATE(pwn)
  DEALLOCATE(umask)
  DEALLOCATE(vmask)
  DEALLOCATE(tmask)
  DEALLOCATE(zind)
  DEALLOCATE(ztfreez)
  DEALLOCATE(rnfmsk)
  DEALLOCATE(upsmsk)
  DEALLOCATE(rnfmsk_z)
  DEALLOCATE(tsn)

end program tra_adv
