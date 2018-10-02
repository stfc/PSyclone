!> Module for describing and storing all information related to
!! a finite-difference grid.
module grid_mod
  use kind_params_mod
  use region_mod
  use gocean_mod
  use subdomain_mod, only: subdomain_type
  implicit none

  private

  ! Enumeration of possible grid types (we only actually
  ! support ARAKAWA_C at the moment)
  integer, public, parameter :: GO_ARAKAWA_C = 0
  integer, public, parameter :: GO_ARAKAWA_B = 1

  ! Enumeration of the four possible choices for 
  ! offsetting the grid-point types relative to the T point.
  !> Points to South and West of T point have same 
  !! i,j index (e.g. 'shallow' code)
  integer, public, parameter :: GO_OFFSET_SW = 0
  integer, public, parameter :: GO_OFFSET_SE = 1
  integer, public, parameter :: GO_OFFSET_NW = 2
  !> Points to North and East of T point have same
  !! i,j index (e.g. NEMO code).
  integer, public, parameter :: GO_OFFSET_NE = 3
  !> Value to signify no dependence on the relative offset of
  !! the different grid-point types.
  integer, public, parameter :: GO_OFFSET_ANY = 4

  ! Enumeration of boundary-condition types
  !> Grid (model domain) has periodic boundary condition
  integer, public, parameter :: GO_BC_PERIODIC = 0
  !> Grid (model domain) has external boundary conditions. This is a
  !! placeholder really as this is a complex area.
  integer, public, parameter :: GO_BC_EXTERNAL = 1
  !> Grid (model domain) has no boundary conditions
  integer, public, parameter :: GO_BC_NONE = 2

  !> The width of the halos we set up for implementing PBCs
  integer, parameter :: HALO_WIDTH_X = 1
  integer, parameter :: HALO_WIDTH_Y = 1

  ! What boundary to align arrays (allocated within the library) to
  ! AVX is 256 bit = 4 d.p. words
  integer, parameter :: ALIGNMENT = 4

  type, public :: grid_type
     !> The type of grid this is (e.g. Arakawa C Grid)
     integer :: name
     !> Specifies the convention by which grid-point
     !! types are indexed relative to a T point.
     integer :: offset
     !> Extent of T-point grid in x. Note that this is the whole grid,
     !! not just the region that is simulated.
     integer :: nx
     !> Extent of T-point grid in y. Note that this is the whole grid,
     !! not just the region that is simulated.
     integer :: ny
     !> Grid spacing in x (m)
     real(go_wp) :: dx
     !> Grid spacing in y (m)
     real(go_wp) :: dy

     !> Nature of each T point: 1 == wet inside simulated region
     !!                         0 == land
     !!                        -1 == wet outside simulated region
     !! This is the key quantity that determines the region that
     !! is actually simulated. However, we also support the
     !! specification of a model consisting entirely of wet points
     !! with periodic boundary conditions. Since this does not
     !! require a T-mask, we do not allocate this array for that
     !! case.
     integer, allocatable :: tmask(:,:)

     !> The type of boundary conditions applied to the model domain
     !! in the x, y and z dimensions. Note that at this stage
     !! this is really only required for Periodic Boundary
     !! conditions.
     integer, dimension(3) :: boundary_conditions

     !> The definition of the subdomain that this process is
     !! responsible for
     type(subdomain_type) :: subdomain

     !> Horizontal scale factors at t point (m)
     real(go_wp), allocatable :: dx_t(:,:), dy_t(:,:)
     !> Horizontal scale factors at u point (m)
     real(go_wp), allocatable :: dx_u(:,:), dy_u(:,:)
     !> Horizontal scale factors at v point (m)
     real(go_wp), allocatable :: dx_v(:,:), dy_v(:,:)  
     !> Horizontal scale factors at f point (m)
     real(go_wp), allocatable :: dx_f(:,:), dy_f(:,:)
     !> Unknown \todo Name these fields!
     real(go_wp), allocatable :: area_t(:,:), area_u(:,:), area_v(:,:)
     !> Latitude of u points
     real(go_wp), allocatable :: gphiu(:,:)
     !> Latitude of v points
     real(go_wp), allocatable :: gphiv(:,:)
     !> Latitude of f points
     real(go_wp), allocatable :: gphif(:,:)

     !> Coordinates of grid (T) points in horizontal plane
     real(go_wp), allocatable :: xt(:,:), yt(:,:)
   contains
     procedure :: get_tmask

  end type grid_type

  interface grid_type
     module procedure grid_constructor
  end interface grid_type

  public grid_init

contains

  !============================================
  function get_tmask(self) result(tmask)
    implicit none
    class (grid_type), target, intent(in) :: self
    integer, pointer :: tmask(:,:)

    tmask => self%tmask

    return
  end function get_tmask

  !> Basic constructor for the grid type. Full details, including domain
  !! decomposition, are fleshed-out by the grid_init() routine.
  function grid_constructor(grid_name, &
                            boundary_conditions, grid_offsets) result(self)
    implicit none
    integer, intent(in) :: grid_name
    !> The boundary conditions that will be applied to all fields that
    !! are defined on this grid.
    integer, dimension(3), intent(in) :: boundary_conditions
    !> The choice of the way the indices of the various grid-point types
    !! are offset from the T point. This is an optional argument as it
    !! will be supplied by PSyclone-modified call of this constructor.
    !! PSyclone will obtain the value of this offset from the kernel
    !! metadata. Despite being optional, it is actually required
    !! and we have a run-time check for this below.
    integer, optional, intent(in) :: grid_offsets
    type(grid_type), target :: self
    ! Locals
    integer :: grid_stagger

    ! We must be told the offset expected by the kernels. In a manual
    ! implementation it is up to the algorithm writer to inspect the
    ! kernels and provide us with the correct value. PSyclone will
    ! automate this process. This argument is only optional in order
    ! to ensure that the code to be processed by PSyclone will
    ! compile.
    if(present(grid_offsets))then
       grid_stagger = grid_offsets
    else
       call gocean_stop('ERROR: grid offset not specified in call to '//&
                        'grid_constructor.')
    end if

    ! This case statement is mainly to check that the caller
    ! has specified a valid value for grid_name.
    select case(grid_name)

    case(GO_ARAKAWA_C)
       self%name = GO_ARAKAWA_C
    case(GO_ARAKAWA_B)
       self%name = GO_ARAKAWA_B
    case default
       write(*,*) 'grid_constructor: ERROR: unsupported grid type: ', &
                  grid_name
       call gocean_stop('')
    end select

    ! Ditto for the choice of how the grid-point types are indexed
    ! relative to the T point
    select case(grid_stagger)

    case(GO_OFFSET_NE)
       self%offset = GO_OFFSET_NE
    case(GO_OFFSET_NW)
       self%offset = GO_OFFSET_NW
    case(GO_OFFSET_SE)
       self%offset = GO_OFFSET_SE
    case(GO_OFFSET_SW)
       self%offset = GO_OFFSET_SW
    case default
       write(*,*) 'grid_constructor: ERROR: unsupported relative offsets of grid types: ', &
                  grid_stagger
       call gocean_stop('')
    end select

    ! Store the boundary conditions that the model domain is
    ! subject to.
    self%boundary_conditions(1:3) = boundary_conditions(1:3)

  end function grid_constructor

  !============================================

  !> Initialise the supplied grid object for a 2D model. The extent
  !! of the model domain is obtained from the supplied decomposition
  !! object.
  !! Ultimately, this routine should be general purpose but it is not
  !! there yet.  For periodic boundary conditions the decomposition
  !! exactly specifies the extent of the simulated region (since we
  !! don't require the user to specify the halos required to
  !! *implement* the PBCs). However, when a T-mask is used to define
  !! the model domain this, of necessity, includes boundary
  !! points. Therefore, the actual simulated region has an extent
  !! which is less than that size of the subdomain in the
  !! decomposition.
  !! @param[inout] grid The object to initialise
  !! @param[in] decomp Decomposition of model - gives us our domain size
  !! @param[in] dxarg Grid spacing in x dimension
  !! @param[in] dyarg Grid spacing in y dimension
  !! @param[in] tmask Array holding the T-point mask which defines
  !!                  the contents of the local domain. Need not be
  !!                  supplied if domain is all wet and has PBCs.
  subroutine grid_init(grid, decomp, dxarg, dyarg, tmask)
    use global_parameters_mod, only: ALIGNMENT
    use subdomain_mod, only: subdomain_type, decomposition_type
    use parallel_mod
    implicit none
    type(grid_type), intent(inout) :: grid
    type(decomposition_type), intent(in) :: decomp
    real(go_wp),        intent(in)    :: dxarg, dyarg
    integer, allocatable, dimension(:,:), intent(in), optional :: tmask
    ! Locals
    integer :: myrank
    integer :: mlocal
    integer :: ierr(5)
    integer :: ji, jj
    integer :: xstart, ystart ! Start of internal region of T-pts
    integer :: xstop, ystop ! End of internal region of T-pts

    ! Copy the definition of the sub-domain for which we are responsible
    ! into our grid object.
    myrank = get_rank()
    grid%subdomain = decomp%subdomains(myrank)

    ! Store the global dimensions of the grid...

    ! Extend the domain by unity in each dimension to allow
    ! for staggering of variables. All fields will be
    ! allocated with extent (nx,ny).
    mlocal = grid%subdomain%global%nx + 1
    if( mod(mlocal, ALIGNMENT) > 0 )then
       ! Since this is the dimension of the array and not that of
       ! the internal region, we add two lots of 'ALIGNMENT'. This
       ! allows us to subsequently extend the loop over the internal
       ! region so that it too is aligned without array accesses of
       ! the form a(i+1,j) going out of bounds.
       grid%nx = (mlocal/ALIGNMENT + 2)*ALIGNMENT
    else
       grid%nx = mlocal
    end if
    grid%ny = grid%subdomain%global%ny + 1

    ! Shorthand for the definition of the internal region
    xstart = grid%subdomain%internal%xstart
    xstop  = grid%subdomain%internal%xstop
    ystart = grid%subdomain%internal%ystart
    ystop  = grid%subdomain%internal%ystop

    ! Copy-in the externally-supplied T-mask, if any. If using OpenMP
    ! then apply first-touch policy for data locality.
    if( present(tmask) )then
       allocate(grid%tmask(grid%nx, grid%ny), stat=ierr(1))
       if( ierr(1) /= 0 )then
          call gocean_stop('grid_init: failed to allocate array for T mask')
       end if
!> TODO should use thread tiling here but that is currently only set-up
!! inside a field object.
!$OMP PARALLEL DO schedule(runtime), default(none), private(ji,jj), &
!$OMP shared(grid, tmask)
       do jj = 1, grid%ny
          do ji = 1, grid%nx
             ! Initially flag all points as being outside the domain
             grid%tmask(ji,jj) = -1
          end do
       end do
!$OMP END PARALLEL DO

       ! Copy of actual values
       grid%tmask(xstart:xstop, ystart:ystop) = tmask(xstart:xstop, &
                                                      ystart:ystop)
    else
       ! No T-mask supplied. Check that grid has PBCs in both
       ! x and y dimensions otherwise we won't know what to do.
       if( .not. ( (grid%boundary_conditions(1) == GO_BC_PERIODIC) .and. &
                   (grid%boundary_conditions(2) == GO_BC_PERIODIC) ) )then
          call gocean_stop('grid_init: ERROR: No T-mask supplied and '// &
                           'grid does not have periodic boundary conditions!')
       end if
       !> TODO add support for PBCs in paralel
       if(get_num_ranks() > 1)then
          call gocean_stop('grid_init: PBCs not yet implemented with MPI')
       end if
    end if ! T-mask supplied

    ! For a regular, orthogonal mesh the spatial resolution is constant
    grid%dx = dxarg
    grid%dy = dyarg

    allocate(grid%dx_t(grid%nx,grid%ny), grid%dy_t(grid%nx,grid%ny), &
             grid%dx_u(grid%nx,grid%ny), grid%dy_u(grid%nx,grid%ny), &
             stat=ierr(1))
    allocate(grid%dx_f(grid%nx,grid%ny), grid%dy_f(grid%nx,grid%ny), &
             grid%dx_v(grid%nx,grid%ny), grid%dy_v(grid%nx,grid%ny), &
             stat=ierr(2)) 
    allocate(grid%area_t(grid%nx,grid%ny), grid%area_u(grid%nx,grid%ny), &
             grid%area_v(grid%nx,grid%ny), stat=ierr(3))
    allocate(grid%gphiu(grid%nx,grid%ny), grid%gphiv(grid%nx,grid%ny), &
             grid%gphif(grid%nx,grid%ny), stat=ierr(4))
    allocate(grid%xt(grid%nx,grid%ny), grid%yt(grid%nx,grid%ny), stat=ierr(5))

    if( any(ierr /= 0, 1) )then
       call gocean_stop('grid_init: failed to allocate arrays')
    end if

    ! Initialise the horizontal scale factors for a regular,
    ! orthogonal mesh. (Constant spatial resolution.)
!$OMP PARALLEL DO schedule(runtime), default(none), private(ji,jj), &
!$OMP shared(grid)
    do jj = 1, grid%ny
       do ji = 1, grid%nx
          grid%dx_t(ji, jj)   = grid%dx
          grid%dy_t(ji, jj)   = grid%dy

          grid%dx_u(ji, jj)   = grid%dx
          grid%dy_u(ji, jj)   = grid%dy

          grid%dx_v(ji, jj)   = grid%dx
          grid%dy_v(ji, jj)   = grid%dy

          grid%dx_f(ji, jj)   = grid%dx
          grid%dy_f(ji, jj)   = grid%dy
       end do
    end do
!$OMP END PARALLEL DO

    ! calculate t,u,v cell area
!$OMP PARALLEL DO schedule(runtime), default(none), private(ji,jj), &
!$OMP shared(grid)
    do jj = 1, grid%ny
       do ji = 1, grid%nx
          grid%area_t(ji,jj) = grid%dx_t(ji,jj) * grid%dy_t(ji,jj)

          grid%area_u(ji,jj) = grid%dx_u(ji,jj) * grid%dy_u(ji,jj)

          grid%area_v(ji,jj) = grid%dx_v(ji,jj) * grid%dy_v(ji,jj)
       END DO
    END DO
!$OMP END PARALLEL DO

    ! -here is an f-plane testing case
    ! i.e. the Coriolis parameter is set to a constant value.
!$OMP PARALLEL DO schedule(runtime), default(none), private(ji,jj), &
!$OMP shared(grid)
    do jj = 1, grid%ny
       do ji = 1, grid%nx
          grid%gphiu(ji, jj) = 50._go_wp
          grid%gphiv(ji, jj) = 50._go_wp
          grid%gphif(ji, jj) = 50._go_wp
       end do
    end do
!$OMP END PARALLEL DO

    ! Co-ordinates of the T points
    ! Do first-touch initialisation before setting actual values
!$OMP PARALLEL DO schedule(runtime), default(none), private(ji,jj), &
!$OMP shared(grid)
    do jj = 1, grid%ny
       do ji = 1, grid%nx
          grid%xt(ji,jj) = 0.0
          grid%yt(ji,jj) = 0.0
       end do
    end do

    grid%xt(xstart, :) = (grid%subdomain%global%xstart - 0.5_go_wp) * &
         grid%dx_t(xstart,:)
    grid%yt(:,ystart)  = (grid%subdomain%global%ystart - 0.5_go_wp) * &
         grid%dy_t(:,ystart)

    DO ji = xstart+1, xstop
      grid%xt(ji,ystart:ystop) = grid%xt(ji-1, ystart:ystop) + grid%dx
    END DO
            
    DO jj = ystart+1, ystop
      grid%yt(xstart:xstop,jj) = grid%yt(xstart:xstop, jj-1) + grid%dy
    END DO

  end subroutine grid_init

  !================================================

end module grid_mod
