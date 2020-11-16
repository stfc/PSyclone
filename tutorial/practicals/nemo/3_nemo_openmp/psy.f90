MODULE tra_adv_mod
  CONTAINS
  SUBROUTINE tra_adv
    USE iso_c_binding, ONLY: C_INT64_T
    INTEGER, PARAMETER :: wp = 8
    REAL(KIND = wp), ALLOCATABLE, SAVE, DIMENSION(:, :, :, :) :: t3sn, t3ns, t3ew, t3we
    REAL(KIND = wp), ALLOCATABLE, SAVE, DIMENSION(:, :, :) :: tsn
    REAL(KIND = wp), ALLOCATABLE, SAVE, DIMENSION(:, :, :) :: pun, pvn, pwn
    REAL(KIND = wp), ALLOCATABLE, SAVE, DIMENSION(:, :, :) :: mydomain, zslpx, zslpy, zwx, zwy, umask, vmask, tmask, zind
    REAL(KIND = wp), ALLOCATABLE, SAVE, DIMENSION(:, :) :: ztfreez, rnfmsk, upsmsk
    REAL(KIND = wp), ALLOCATABLE, SAVE, DIMENSION(:) :: rnfmsk_z
    REAL(KIND = wp) :: zice, zu, z0u, zzwx, zv, z0v, zzwy, ztra, zbtr, zdt, zalpha
    REAL(KIND = wp) :: r
    REAL(KIND = wp) :: zw, z0w
    INTEGER :: jpi, jpj, jpk, ji, jj, jk, jt
    INTEGER(KIND = C_INT64_T) :: it
    CHARACTER(LEN = 10) :: env
    INTEGER :: init_timer, step_timer
    CALL get_environment_variable("JPI", env)
    READ(env, FMT = '(i10)') jpi
    CALL get_environment_variable("JPJ", env)
    READ(env, FMT = '(i10)') jpj
    CALL get_environment_variable("JPK", env)
    READ(env, FMT = '(i10)') jpk
    CALL get_environment_variable("IT", env)
    READ(env, FMT = '(i10)') it
    IF (jpi < 1) STOP 'Width of grid, JPI, must be > 0'
    IF (jpj < 1) STOP 'Height of grid, JPJ, must be > 0'
    IF (jpk < 1) STOP 'Depth of grid, JPK, must be > 0'
    IF (it < 1) STOP 'Number of iterations, IT, must be > 0'
    WRITE(*, FMT = "('Tracer-advection Mini-app:')")
    WRITE(*, FMT = "('Domain is ', I4, ' x', I4, ' grid points with ', I3,  ' vertical levels')") jpi, jpj, jpk
    WRITE(*, FMT = "('Performing ', I4, ' iterations')") it
    ALLOCATE(mydomain(jpi, jpj, jpk))
    ALLOCATE(zwx(jpi, jpj, jpk))
    ALLOCATE(zwy(jpi, jpj, jpk))
    ALLOCATE(zslpx(jpi, jpj, jpk))
    ALLOCATE(zslpy(jpi, jpj, jpk))
    ALLOCATE(pun(jpi, jpj, jpk))
    ALLOCATE(pvn(jpi, jpj, jpk))
    ALLOCATE(pwn(jpi, jpj, jpk))
    ALLOCATE(umask(jpi, jpj, jpk))
    ALLOCATE(vmask(jpi, jpj, jpk))
    ALLOCATE(tmask(jpi, jpj, jpk))
    ALLOCATE(zind(jpi, jpj, jpk))
    ALLOCATE(ztfreez(jpi, jpj))
    ALLOCATE(rnfmsk(jpi, jpj))
    ALLOCATE(upsmsk(jpi, jpj))
    ALLOCATE(rnfmsk_z(jpk))
    ALLOCATE(tsn(jpi, jpj, jpk))
    r = jpi * jpj * jpk
    !$OMP parallel default(shared), private(ji,jj,jk)
    !$OMP do schedule(static)
    DO jk = 1, jpk
      DO jj = 1, jpj
        DO ji = 1, jpi
          umask(ji, jj, jk) = ji * jj * jk / r
          mydomain(ji, jj, jk) = ji * jj * jk / r
          pun(ji, jj, jk) = ji * jj * jk / r
          pvn(ji, jj, jk) = ji * jj * jk / r
          pwn(ji, jj, jk) = ji * jj * jk / r
          vmask(ji, jj, jk) = ji * jj * jk / r
          tsn(ji, jj, jk) = ji * jj * jk / r
          tmask(ji, jj, jk) = ji * jj * jk / r
        END DO
      END DO
    END DO
    !$OMP end do
    !$OMP end parallel
    r = jpi * jpj
    DO jj = 1, jpj
      DO ji = 1, jpi
        ztfreez(ji, jj) = ji * jj / r
        upsmsk(ji, jj) = ji * jj / r
        rnfmsk(ji, jj) = ji * jj / r
      END DO
    END DO
    !$OMP parallel default(shared), private(jk)
    !$OMP do schedule(static)
    DO jk = 1, jpk
      rnfmsk_z(jk) = jk / jpk
    END DO
    !$OMP end do
    !$OMP end parallel
    DO jt = 1, it
      !$OMP parallel default(shared), private(ji,jj,jk,zice)
      !$OMP do schedule(static)
      DO jk = 1, jpk
        DO jj = 1, jpj
          DO ji = 1, jpi
            IF (tsn(ji, jj, jk) <= ztfreez(ji, jj) + 0.1D0) THEN
              zice = 1.D0
            ELSE
              zice = 0.D0
            END IF
            zind(ji, jj, jk) = MAX(rnfmsk(ji, jj) * rnfmsk_z(jk), upsmsk(ji, jj), zice) * tmask(ji, jj, jk)
            zind(ji, jj, jk) = 1 - zind(ji, jj, jk)
          END DO
        END DO
      END DO
      !$OMP end do
      !$OMP end parallel
      zwx(:, :, jpk) = 0.E0
      zwy(:, :, jpk) = 0.E0
      !$OMP parallel default(shared), private(ji,jj,jk)
      !$OMP do schedule(static)
      DO jk = 1, jpk - 1
        DO jj = 1, jpj - 1
          DO ji = 1, jpi - 1
            zwx(ji, jj, jk) = umask(ji, jj, jk) * (mydomain(ji + 1, jj, jk) - mydomain(ji, jj, jk))
            zwy(ji, jj, jk) = vmask(ji, jj, jk) * (mydomain(ji, jj + 1, jk) - mydomain(ji, jj, jk))
          END DO
        END DO
      END DO
      !$OMP end do
      !$OMP end parallel
      zslpx(:, :, jpk) = 0.E0
      zslpy(:, :, jpk) = 0.E0
      !$OMP parallel default(shared), private(ji,jj,jk,z0u,z0v,zalpha,zbtr,zdt,ztra,zu,zv,zzwx,zzwy)
      !$OMP do schedule(static)
      DO jk = 1, jpk - 1
        DO jj = 2, jpj
          DO ji = 2, jpi
            zslpx(ji, jj, jk) = (zwx(ji, jj, jk) + zwx(ji - 1, jj, jk)) * (0.25D0 + SIGN(0.25D0, zwx(ji, jj, jk) * zwx(ji - 1, jj, &
&jk)))
            zslpy(ji, jj, jk) = (zwy(ji, jj, jk) + zwy(ji, jj - 1, jk)) * (0.25D0 + SIGN(0.25D0, zwy(ji, jj, jk) * zwy(ji, jj - 1, &
&jk)))
          END DO
        END DO
      END DO
      !$OMP end do
      !$OMP do schedule(static)
      DO jk = 1, jpk - 1
        DO jj = 2, jpj
          DO ji = 2, jpi
            zslpx(ji, jj, jk) = SIGN(1.D0, zslpx(ji, jj, jk)) * MIN(ABS(zslpx(ji, jj, jk)), 2.D0 * ABS(zwx(ji - 1, jj, jk)), 2.D0 &
&* ABS(zwx(ji, jj, jk)))
            zslpy(ji, jj, jk) = SIGN(1.D0, zslpy(ji, jj, jk)) * MIN(ABS(zslpy(ji, jj, jk)), 2.D0 * ABS(zwy(ji, jj - 1, jk)), 2.D0 &
&* ABS(zwy(ji, jj, jk)))
          END DO
        END DO
      END DO
      !$OMP end do
      !$OMP do schedule(static)
      DO jk = 1, jpk - 1
        zdt = 1
        DO jj = 2, jpj - 1
          DO ji = 2, jpi - 1
            z0u = SIGN(0.5D0, pun(ji, jj, jk))
            zalpha = 0.5D0 - z0u
            zu = z0u - 0.5D0 * pun(ji, jj, jk) * zdt
            zzwx = mydomain(ji + 1, jj, jk) + zind(ji, jj, jk) * (zu * zslpx(ji + 1, jj, jk))
            zzwy = mydomain(ji, jj, jk) + zind(ji, jj, jk) * (zu * zslpx(ji, jj, jk))
            zwx(ji, jj, jk) = pun(ji, jj, jk) * (zalpha * zzwx + (1. - zalpha) * zzwy)
            z0v = SIGN(0.5D0, pvn(ji, jj, jk))
            zalpha = 0.5D0 - z0v
            zv = z0v - 0.5D0 * pvn(ji, jj, jk) * zdt
            zzwx = mydomain(ji, jj + 1, jk) + zind(ji, jj, jk) * (zv * zslpy(ji, jj + 1, jk))
            zzwy = mydomain(ji, jj, jk) + zind(ji, jj, jk) * (zv * zslpy(ji, jj, jk))
            zwy(ji, jj, jk) = pvn(ji, jj, jk) * (zalpha * zzwx + (1.D0 - zalpha) * zzwy)
          END DO
        END DO
      END DO
      !$OMP end do
      !$OMP do schedule(static)
      DO jk = 1, jpk - 1
        DO jj = 2, jpj - 1
          DO ji = 2, jpi - 1
            zbtr = 1.
            ztra = - zbtr * (zwx(ji, jj, jk) - zwx(ji - 1, jj, jk) + zwy(ji, jj, jk) - zwy(ji, jj - 1, jk))
            mydomain(ji, jj, jk) = mydomain(ji, jj, jk) + ztra
          END DO
        END DO
      END DO
      !$OMP end do
      !$OMP end parallel
      zwx(:, :, 1) = 0.E0
      zwx(:, :, jpk) = 0.E0
      !$OMP parallel default(shared), private(jk)
      !$OMP do schedule(static)
      DO jk = 2, jpk - 1
        zwx(:, :, jk) = tmask(:, :, jk) * (mydomain(:, :, jk - 1) - mydomain(:, :, jk))
      END DO
      !$OMP end do
      !$OMP end parallel
      zslpx(:, :, 1) = 0.E0
      !$OMP parallel default(shared), private(ji,jj,jk)
      !$OMP do schedule(static)
      DO jk = 2, jpk - 1
        DO jj = 1, jpj
          DO ji = 1, jpi
            zslpx(ji, jj, jk) = (zwx(ji, jj, jk) + zwx(ji, jj, jk + 1)) * (0.25D0 + SIGN(0.25D0, zwx(ji, jj, jk) * zwx(ji, jj, jk &
&+ 1)))
          END DO
        END DO
      END DO
      !$OMP end do
      !$OMP do schedule(static)
      DO jk = 2, jpk - 1
        DO jj = 1, jpj
          DO ji = 1, jpi
            zslpx(ji, jj, jk) = SIGN(1.D0, zslpx(ji, jj, jk)) * MIN(ABS(zslpx(ji, jj, jk)), 2.D0 * ABS(zwx(ji, jj, jk + 1)), 2.D0 &
&* ABS(zwx(ji, jj, jk)))
          END DO
        END DO
      END DO
      !$OMP end do
      !$OMP end parallel
      zwx(:, :, 1) = pwn(:, :, 1) * mydomain(:, :, 1)
      zdt = 1
      zbtr = 1.
      !$OMP parallel default(shared), private(ji,jj,jk,z0w,zalpha,zw,zzwx,zzwy)
      !$OMP do schedule(static)
      DO jk = 1, jpk - 1
        DO jj = 2, jpj - 1
          DO ji = 2, jpi - 1
            z0w = SIGN(0.5D0, pwn(ji, jj, jk + 1))
            zalpha = 0.5D0 + z0w
            zw = z0w - 0.5D0 * pwn(ji, jj, jk + 1) * zdt * zbtr
            zzwx = mydomain(ji, jj, jk + 1) + zind(ji, jj, jk) * (zw * zslpx(ji, jj, jk + 1))
            zzwy = mydomain(ji, jj, jk) + zind(ji, jj, jk) * (zw * zslpx(ji, jj, jk))
            zwx(ji, jj, jk + 1) = pwn(ji, jj, jk + 1) * (zalpha * zzwx + (1. - zalpha) * zzwy)
          END DO
        END DO
      END DO
      !$OMP end do
      !$OMP end parallel
      zbtr = 1.
      !$OMP parallel default(shared), private(ji,jj,jk,ztra)
      !$OMP do schedule(static)
      DO jk = 1, jpk - 1
        DO jj = 2, jpj - 1
          DO ji = 2, jpi - 1
            ztra = - zbtr * (zwx(ji, jj, jk) - zwx(ji, jj, jk + 1))
            mydomain(ji, jj, jk) = ztra
          END DO
        END DO
      END DO
      !$OMP end do
      !$OMP end parallel
    END DO
    OPEN(UNIT = 4, FILE = 'output.dat', FORM = 'formatted')
    DO jk = 1, jpk - 1
      DO jj = 2, jpj - 1
        DO ji = 2, jpi - 1
          WRITE(4, FMT = *) mydomain(ji, jj, jk)
        END DO
      END DO
    END DO
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
    WRITE(*, FMT = "('Mini-app finished.')")
  END SUBROUTINE tra_adv
END MODULE tra_adv_mod