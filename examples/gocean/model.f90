MODULE model
  USE field
  USE mesh
  USE shallow_IO
  USE timing, ONLY: timer_init, timer_report
  IMPLICIT none

  INTEGER :: m, n      !< global domain size (no. of grid pts)
  INTEGER :: mp1, np1  !< m+1 and n+1 == array extents

  INTEGER :: itmax   !< number of timesteps

  TYPE(scalar_field_type) :: dt  !< model timestep (seconds)
!  REAL(KIND=8) :: dt
  TYPE(scalar_field_type) :: tdt !< 2xdt apart from first step when is just dt
!  REAL(KIND=8) :: tdt 

  ! solution arrays
  ! Fields are allocated with extents (M+1,N+1).
  ! Presumably the extra row and column are needed for periodic BCs.

  ! The finite-difference variables are defined on the mesh as follows:
  !            u              u          
  !    -------->-----P,H------>-----P,H
  !    |              |              |   
  !    |        rot   |        rot   |   
  !v  /|\      o     /|\ v    o     /|\ v
  !    |              |              |   
  !    |              |              |   
  !    -------->-----P,H------>-----P,H
  !    |              |              |   
  !    |        rot   |        rot   |   
  !v  /|\      o     /|\ v    o     /|\ v
  !    |              |              |   
  !    |              |              |   
  !    |------->------|------->------|   
  !            u              u          

  ! Calling the point at which P,H etc. are defined 'T' and the
  ! point at which rotation/vorticity defined 'f' then:
  !
  !          f(i,j+1) v(i,j+1)  f(i+1,j+1)  v(i+1,j+1)  f(i+2,j+1)
  !
  !          u(i,j)   T(i,j)    u(i+1,j)    T(i+1,j)    u(i+2,j)
  !
  !          f(i,j)   v(i,j)    f(i+1,j)    v(i+1,j)    f...
  !
  !          u(i,j-1) T(i,j-1)  u(i+1,j-1)  T...        u...
  !
  !  So, T is dual of f.
  !
  ! This is consistent with the form of the original code:
  !     DO J=1,N
  !        DO I=1,M
  !           CU(I+1,J) = .5*(P(I+1,J)+P(I,J))*U(I+1,J)
  !           CV(I,J+1) = .5*(P(I,J+1)+P(I,J))*V(I,J+1)
  !           Z(I+1,J+1) = &
  !              (FSDX*(V(I+1,J+1)-V(I,J+1))-FSDY*(U(I+1,J+1)-U(I+1,J))) &
  !                          /(P(I,J)+P(I+1,J)+P(I+1,J+1)+P(I,J+1))
  !           H(I,J) = P(I,J)+.25*(U(I+1,J)*U(I+1,J)+U(I,J)*U(I,J)     & 
  !                +V(I,J+1)*V(I,J+1)+V(I,J)*V(I,J))
  !        END DO
  !     END DO

  !> Potential Enstrophy. Is this defined on the same mesh
  !! points as the vorticity?
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: z
  !> Component of vel in x at current time step
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: u
  !> Component of vel in x at next time step
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: unew
  !> Component of vel in x at previous time step
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: uold
  !> Component of vel in y current time step
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: v
  !> Component of vel in y next time step
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: vnew
  !> Component of vel in y previous time step
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: vold
  !> Pressure at current time step
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: p
  !> Pressure at next time step
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: pnew
  !> Pressure at previous time step
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: pold
  !> Mass flux in x at u point
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: cu
  !> Mass flux in y at v point
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: cv
  !> H = P + 0.5(<u^2>_x + <v^2>_y), defined on the same
  !! grid points as the pressure, P
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: h
  !> Stream function
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: psi  

CONTAINS

  !================================================

  SUBROUTINE model_init()
    USE mesh, ONLY: set_grid_extents, set_grid_spacings
    USE time_smooth, ONLY: time_smooth_init
    IMPLICIT none
    !> Grid spacings currently hard-wired, as in original
    !! version of code.
    REAL(KIND=8), PARAMETER :: dxloc=1.0E5, dyloc=1.0E5
    !> Parameter for time smoothing
    REAL(KIND=8), PARAMETER :: alpha_loc = .001
    !> Hardwired model time-step (seconds)
    REAL(KIND=8), PARAMETER :: dt_loc = 90.

    CALL timer_init()

    CALL read_namelist(m,n,itmax)

    ! Set up mesh parameters
    CALL set_grid_extents(m, n)
    mp1 = m + 1
    np1 = n + 1

    CALL set_grid_spacings(dxloc, dyloc)

    ! Allocate model arrays
    CALL model_alloc(mp1, np1)

    ! Set model time-step
    CALL set(dt, dt_loc)

    ! Initialise time-smoothing module
    CALL time_smooth_init(alpha_loc)

    ! Initialise model IO 'system'
    CALL model_write_init(m,n)

    ! Log model parameters
    CALL print_initial_values(m,n,dxloc,dyloc, dt%data, alpha_loc)

  END SUBROUTINE model_init

  !================================================

  SUBROUTINE model_finalise()
    IMPLICIT none

    CALL model_write_finalise()

    CALL timer_report()

    CALL model_dealloc()
  
  END SUBROUTINE model_finalise

  !================================================

  SUBROUTINE model_alloc(idimx, idimy)
    IMPLICIT none
    INTEGER, INTENT(in) :: idimx, idimy

    ALLOCATE( u(idimx,idimy),    v(idimx,idimy),    p(idimx,idimy) ) 
    ALLOCATE( unew(idimx,idimy), vnew(idimx,idimy), pnew(idimx,idimy) ) 
    ALLOCATE( uold(idimx,idimy), vold(idimx,idimy), pold(idimx,idimy) )
    ALLOCATE( cu(idimx,idimy),   cv(idimx,idimy) ) 
    ALLOCATE( z(idimx,idimy),    h(idimx,idimy),    psi(idimx,idimy) ) 

  END SUBROUTINE model_alloc

  !================================================

  SUBROUTINE model_dealloc()
    IMPLICIT none

    !> Free memory \todo Move to model_finalise()
    DEALLOCATE( u, v, p, unew, vnew, pnew, uold, vold, pold )
    DEALLOCATE( cu, cv, z, h, psi ) 

  END SUBROUTINE model_dealloc

END MODULE model
