MODULE initial_conditions
  USE physical_params
  USE field
  IMPLICIT none
  PRIVATE

  !> Amplitude of initial oscillations in stream function
  !! Used by invoke_init_stream_fn_kernel()
  REAL(wp), PARAMETER :: A = 1.0E6
  !> 2PI/{m,n}
  REAL(wp)  :: di, dj

  PUBLIC init_initial_condition_params
  PUBLIC invoke_init_stream_fn_kernel
  PUBLIC init_pressure
  PUBLIC init_velocity_u
  PUBLIC init_velocity_v

CONTAINS

  !===================================================

  !> \brief Set-up parameters related to the model domain which
  !! are stored in this module. We could compute these on the
  !! fly in init_stream_fn_code() and init_pressure() and
  !! rely on compiler magic to make sure they're not
  !! recomputed for every grid point.
  SUBROUTINE init_initial_condition_params()
    USE model, ONLY: m, n
    IMPLICIT none

    di = TPI/m
    dj = TPI/n


  END SUBROUTINE init_initial_condition_params

  !===================================================

  SUBROUTINE invoke_init_stream_fn_kernel(psi)
    IMPLICIT none
    REAL(KIND=8), INTENT(out), DIMENSION(:,:) :: psi
    ! Locals
    INTEGER :: idim1, idim2
    INTEGER :: i, j

    idim1 = SIZE(psi, 1)
    idim2 = SIZE(psi, 2)

    ! Loop over 'columns'
    DO J=1, idim2
      DO I=1, idim1

        CALL init_stream_fn_code(i, j, psi)

      END DO
    END DO

  CONTAINS

    SUBROUTINE init_stream_fn_code(i, j, psi)
      IMPLICIT none
      !> The grid point (column) to act on
      INTEGER,      INTENT(in)                  :: i, j
      !> Array holding the stream function values
      REAL(KIND=8), INTENT(out), DIMENSION(:,:) :: psi

      ! di = 2Pi/(Extent of mesh in x)
      ! dj = 2Pi/(Extent of mesh in y)

      PSI(I,J) = A*SIN((I-.5)*DI)*SIN((J-.5)*DJ)

    END SUBROUTINE init_stream_fn_code

  END SUBROUTINE invoke_init_stream_fn_kernel

  !===================================================

  SUBROUTINE init_pressure(p)
    USE model, ONLY: m, dx
    IMPLICIT none
    REAL(KIND=8), INTENT(out), DIMENSION(:,:) :: p
!    TYPE(field_type), INTENT(out) :: p
    ! Locals
    INTEGER :: idim1, idim2
    INTEGER :: i, j
    !> Extent in x of model domain
    REAL(wp) :: el
    !> Computed amplitude of initial osciallations in
    !! pressure field.
    REAL(wp) :: pcf

    idim1 = SIZE(p, 1)
    idim2 = SIZE(p, 2)

    EL = m*dx
    PCF = PI*PI*A*A/(EL*EL)

    ! di = 2Pi/(Extent of mesh in x)
    ! dj = 2Pi/(Extent of mesh in y)
    ! Fields have one extra row and column than the mesh
    ! extent. Presumably because e.g. velocity fields are
    ! offset from pressure but this not dealt with that
    ! explicitly. Only field(1:m,1:n) is sent to be
    ! written to the netcdf file.
    DO J=1,idim2
       DO I=1,idim1
          P(I,J) = PCF*(COS(2.*(I-1)*DI)   & 
               +COS(2.*(J-1)*DJ))+50000.
       END DO
    END DO

  END SUBROUTINE init_pressure

  !===================================================

  SUBROUTINE init_velocity_u(u, psi, m, n)
    USE mesh, ONLY: dy
    IMPLICIT none
    REAL(KIND=8), INTENT(out), DIMENSION(:,:) :: u
    REAL(KIND=8), INTENT(in),  DIMENSION(:,:) :: psi
    INTEGER,      INTENT(in) :: m, n
    ! Locals
    INTEGER :: I, J

    ! dy is a property of the mesh
    DO J=1,N
       DO I=1,M+1
          U(I,J) = -(PSI(I,J+1)-PSI(I,J))/dy
       END DO
    END DO
  END SUBROUTINE init_velocity_u

  !===================================================

  SUBROUTINE init_velocity_v(v, psi, m, n)
    USE mesh, ONLY: dx
    IMPLICIT none
    REAL(KIND=8), INTENT(out), DIMENSION(:,:) :: v
    REAL(KIND=8), INTENT(in),  DIMENSION(:,:) :: psi
    INTEGER, INTENT(in) :: m, n
    ! Locals
    INTEGER :: I, J

    ! dx is a property of the mesh
    DO J=1,N+1
       DO I=1,M
          V(I,J) = (PSI(I+1,J)-PSI(I,J))/DX
       END DO
    END DO
  END SUBROUTINE init_velocity_v

END MODULE initial_conditions
