module field_mod
  use kind_params_mod
  use region_mod
  use halo_mod
  use grid_mod
  use gocean_mod, only: gocean_stop
  implicit none

  private

  ! Enumeration of grid-point types on the Arakawa C grid. A
  ! field lives on one of these types.
  integer, public, parameter :: U_POINTS   = 0
  integer, public, parameter :: V_POINTS   = 1
  integer, public, parameter :: T_POINTS   = 2
  integer, public, parameter :: F_POINTS   = 3
  !> A field that lives on all grid-points of the grid
  integer, public, parameter :: ALL_POINTS = 4

  !> A field is sub-divided into tiles for coarse-grained OpenMP
  type :: tile_type
     !> Tile region to use when only a field's 'internal'
     !! points are required
     type(region_type) :: internal
     !> Tile region to use when all of a field's points are required
     type(region_type) :: whole
     ! Could potentially physically divide up an array into 
     ! distinct tiles and store the data for each separately...
     !real(wp), dimension(:,:), allocatable :: data
  end type tile_type

  !> The base field type. Intended to represent a global field
  !! such as the x-component of velocity.
  type, public :: field_type
     !> Which mesh points the field is defined upon
     integer :: defined_on
     !> The grid on which this field is defined
     type(grid_type), pointer :: grid
     !> The internal region of this field
     type(region_type) :: internal
     !> The whole region covered by this field - includes 
     !! boundary points
     type(region_type) :: whole
     !> The number of halo regions that this field has.
     !! Halo region values are not computed but copied
     !! from elsewhere.
     integer :: num_halos
     !> Array of objects describing the halos belonging to this field.
     type(halo_type), dimension(:), allocatable :: halo
  end type field_type

  type, public, extends(field_type) :: scalar_field
     real(wp) :: data
  end TYPE scalar_field

  !> A real, 2D field.
  type, public, extends(field_type) :: r2d_field
     integer :: ntiles
     !> The dimensions of the tiles into which the field
     !! is sub-divided.
     type(tile_type), dimension(:), allocatable :: tile
     !> Array holding the actual field values
     real(wp), dimension(:,:), allocatable :: data
  end type r2d_field

  !interface set_field
  !   module procedure set_scalar_field
  !end interface set_field

  !> Interface for the copy_field operation. Overloaded to take
  !! a scalar, an array or an r2d_field type.
  !! \todo Remove support for raw arrays from this interface.
  interface copy_field
     module procedure copy_scalar_field,                            &
                      copy_2dfield_array, copy_2dfield_array_patch, &
                      copy_2dfield, copy_2dfield_patch
  end interface copy_field

  interface increment_field
     module procedure increment_scalar_field, increment_scalar_field_r8
  end interface increment_field

!  interface field_type
!     module procedure field_constructor
!  end interface field_type

  ! User-defined constructor for r2d_field type objects
  interface r2d_field
     module procedure r2d_field_constructor
  end interface r2d_field

  !> Interface for the field checksum operation. Overloaded to take either
  !! a field object or a 2D, real(wp) array.
  interface field_checksum
     module procedure fld_checksum, array_checksum
  end interface field_checksum

  !> Info on the tile sizes
  INTEGER, SAVE :: max_tile_width
  INTEGER, SAVE :: max_tile_height

  public increment_field
  public copy_field
  public set_field
  public field_checksum

! Grid points on an Arakawa C grid with NE offset (i.e. the U,V and F pts
! immediately to the North and East of a T point share its grid indices) 
! are arranged like so:
!
!v(1,ny)----f(1,ny)---v(i-1,ny)--f(i-1,ny)--v(i,ny)----f(i,ny)--v(nx,ny)---f(nx,ny)  
!|          |         |          |          |          |        |          |        
!|          |         |          |          |          |        |          |        
!T[1,ny]----u(1,ny)---T(i-1,ny)--u(i-1,ny)--T(i,ny)----u(i,ny)--T(nx,ny)---u(nx,ny)  
!|          |         |          |          |          |        |          |        
!|          |         |          |          |          |        |          |        
!v(1,j)-----f(1,j)----v(i-1,j)---f(i-1,j)---v(i,j)-----f(i,j)---v(nx,j)----f(nx,j)   
!|          |         |          |          |          |        |          |        
!|          |         |          |          |          |        |          |        
!T[1,j]-----u(1,j)----T(i-1,j)---u(i-1,j)---T(i,j)-----u(i,j)---T(nx,j)----u(nx,j)   
!|          |         |          |          |          |        |          |        
!|          |         |          |          |          |        |          |        
!v(1,j-1)---f(1,j-1)--v(i-1,j-1)-f(i-1,j-1)-v(i,j-1)---f(i,j-1)-v(nx,j-1)--f(nx,j-1) 
!|          |         |          |          |          |        |          |        
!|          |         |          |          |          |        |          |        
!T[1,j-1]---u(1,j-1)--T(i-1,j-1)-u(i-1,j-1)-T(i,j-1)---u(i,j-1)-T(nx,j-1)--u(nx,j-1) 
!|          |         |          |          |          |        |          |        
!|          |         |          |          |          |        |          |        
!v(1,1)-----f(1,1)----v(i-1,1)---f(i-1,1)---v(i,1)-----f(i,1)---v(nx,1)----f(nx,1)   
!|          |         |          |          |          |        |          |        
!|          |         |          |          |          |        |          |        
!T[1,1]     u(1,1)    T(i-1,1)---u(i-1,1)---T(i,1)-----u(i,1)---T(nx,1)----u(nx,1)   

  !> The no. of cols/rows used to define boundary data in the absence
  !! of periodic BCs.
  !! \todo remove this parameter and determine it from
  !! the supplied T mask.
  integer, public, parameter :: NBOUNDARY = 1

  !> Whether or not to 'tile' (sub-divide) field arrays
  logical, public, parameter :: TILED_FIELDS = .TRUE.

contains

  !===================================================

  function r2d_field_constructor(grid,    &
                                 grid_points) result(self)
    implicit none
    ! Arguments
    !> Pointer to the grid on which this field lives
    type(grid_type), intent(in), target  :: grid
    !> Which grid-point type the field is defined on
    integer,         intent(in)          :: grid_points
    ! Local declarations
    type(r2d_field) :: self
    integer :: ierr
    character(len=8) :: fld_type
    integer :: ji, jj
    !> The upper bounds actually used to allocate arrays (as opposed
    !! to the limits carried around with the field)
    integer :: upper_x_bound, upper_y_bound

    ! Set this field's grid pointer to point to the grid pointed to
    ! by the supplied grid_ptr argument
    self%grid => grid

    ! Set-up the limits of the 'internal' region of this field
    !
    call set_field_bounds(self,fld_type,grid_points)

    call tile_setup(self)

    ! We allocate all fields to have the same extent as that
    ! with the greatest extents. This enables the (Cray) compiler
    ! to safely evaluate code within if blocks that are
    ! checking for conditions at the boundary of the domain.
    ! Hence we use self%whole%{x,y}stop + 1...
    !> \todo Implement calculation of largest array extents
    !! required by any field type rather than hard-wiring
    !! a simple increase of each extent.
    upper_x_bound = self%whole%xstop + 1
    upper_y_bound = self%whole%ystop + 1

    write(*,"('Allocating ',(A),' field with bounds: (',I1,':',I3, ',',I1,':',I3,')')") &
               TRIM(ADJUSTL(fld_type)), &
               1, upper_x_bound, 1, upper_y_bound

    !allocate(self%data(self%internal%xstart-1:self%internal%xstop+1, &
    !                   self%internal%ystart-1:self%internal%ystop+1),&
    !                   Stat=ierr)
    ! Allocating with a lower bound != 1 causes problems whenever
    ! array passed as assumed-shape dummy argument because lower
    ! bounds default to 1 in called unit.
    ! However, all loops will be in the generated, middle layer and
    ! the generator knows the array bounds. This may give us the
    ! ability to solve this problem (by passing array bounds to the
    ! kernels).
    allocate(self%data(1:upper_x_bound, 1:upper_y_bound), &
                       Stat=ierr)
    if(ierr /= 0)then
       call gocean_stop('r2d_field_constructor: ERROR: failed to '// &
                        'allocate field')
    end if

    ! Since we're allocating the arrays to be larger than strictly
    ! required we explicitly set all elements to -999 in case the code
    ! does access 'out-of-bounds' elements during speculative
    ! execution. If we're running with OpenMP this also gives
    ! us the opportunity to do a 'first touch' policy to aid with
    ! memory<->thread locality...
!$OMP PARALLEL DO schedule(runtime), default(none), &
!$OMP private(ji,jj), shared(self, upper_x_bound, upper_y_bound)
    do jj = 1, upper_y_bound, 1
       do ji = 1, upper_x_bound, 1
          self%data(ji,jj) = -999.0
       end do
    end do
!$OMP END PARALLEL DO

  end function r2d_field_constructor

  !===================================================

  subroutine set_field_bounds(fld, fld_type, grid_points)
    implicit none
    !> The field we're working on. We use class as we want this
    ! to be polymorphic as this routine doesn't care whether the
    ! field is tiled or not.
    class(field_type), intent(inout) :: fld
    !> Which grid-point type the field is defined on
    integer,          intent(in)    :: grid_points
    !> Character string describing the grid-point type
    character(len=8), intent(out)   :: fld_type

    select case(grid_points)

    case(U_POINTS)
       write(fld_type, "('C-U')")
       call cu_field_init(fld)
    case(V_POINTS)
       write(fld_type, "('C-V')")
       call cv_field_init(fld)
    case(T_POINTS)
       write(fld_type, "('C-T')")
       call ct_field_init(fld)
    case(F_POINTS)
       write(fld_type, "('C-F')")
       call cf_field_init(fld)
    case(ALL_POINTS)
       write(fld_type, "('C-All')")
       call field_init(fld)
    case default
       call gocean_stop('r2d_field_constructor: ERROR: invalid '//&
                        'specifier for type of mesh points')
    end select

    ! Compute and store dimensions of internal region of field
    fld%internal%nx = fld%internal%xstop - fld%internal%xstart + 1
    fld%internal%ny = fld%internal%ystop - fld%internal%ystart + 1

    ! In addition to the 'internal region' of the field, we may have
    ! external points that define B.C.'s or that act as halos. Here
    ! we store the full extent of the field, inclusive of such
    ! points.
    !> \todo Replace the use of NBOUNDARY here with info. computed
    !! from the T-point mask.
    if(fld%grid%boundary_conditions(1) /= BC_PERIODIC)then
       fld%whole%xstart = fld%internal%xstart - NBOUNDARY
       fld%whole%xstop  = fld%internal%xstop  + NBOUNDARY
    else
       fld%whole%xstart = fld%internal%xstart - NBOUNDARY
       fld%whole%xstop  = fld%internal%xstop  + NBOUNDARY
    end if
    if(fld%grid%boundary_conditions(2) /= BC_PERIODIC)then
       fld%whole%ystart = fld%internal%ystart - NBOUNDARY
       fld%whole%ystop  = fld%internal%ystop  + NBOUNDARY
    else
       fld%whole%ystart = fld%internal%ystart - NBOUNDARY
       fld%whole%ystop  = fld%internal%ystop  + NBOUNDARY
    end if

    fld%whole%nx = fld%whole%xstop - fld%whole%xstart + 1
    fld%whole%ny = fld%whole%ystop - fld%whole%ystart + 1

  end subroutine set_field_bounds

  !===================================================

  subroutine field_init(fld)
    implicit none
    class(field_type), intent(inout) :: fld
    ! Locals
    integer :: M, N

    fld%defined_on = ALL_POINTS

    M = fld%grid%nx
    N = fld%grid%ny

    ! An 'all points' field is defined upon every point in the grid
    fld%internal%xstart = 1
    fld%internal%xstop  = M
    fld%internal%ystart = 1
    fld%internal%ystop  = N

    ! We have no halo regions
    fld%num_halos = 0

  end subroutine field_init

  !===================================================

  subroutine cu_field_init(fld)
    implicit none
    class(field_type), intent(inout) :: fld

    fld%defined_on = U_POINTS

    select case(fld%grid%offset)

    case(OFFSET_SW)
       call cu_sw_init(fld)

    case(OFFSET_NE)
       call cu_ne_init(fld)

    case default
       call gocean_stop('cu_field_init: ERROR - unsupported grid offset!')

    end select

  end subroutine cu_field_init

  !===================================================

  subroutine cu_sw_init(fld)
    implicit none
    class(field_type), intent(inout) :: fld

    ! Set up a field defined on U points when the grid has
    ! a South-West offset:

    !   vi-1j+1--fij+1---vij+1---fi+1j+1
    !   |        |       |       |
    !   |        |       |       |
    !   Ti-1j----uij-----Tij-----ui+1j
    !   |        |       |       |
    !   |        |       |       |
    !   vi-1j----fij-----vij-----fi+1j
    !   |        |       |       |
    !   |        |       |       |
    !   Ti-1j-1--uij-1---Tij-1---ui+1j-1

    !
    if(fld%grid%boundary_conditions(1) == BC_PERIODIC)then
       ! When implementing periodic boundary conditions, all mesh
       ! point types have the same extents as the grid of T points. We
       ! then have a halo of width grid_mod::HALO_WIDTH_X on either
       ! side of the domain.
       ! When updating a quantity on U points we write to:
       ! (using 'x' to indicate a location that is written and 'a' an
       ! additional point that shallow dispenses with):
       !
       ! i=istart i=istop
       !  o  o  o  o  a
       !  o  x  x  x  a  j=ystop
       !  o  x  x  x  a
       !  o  x  x  x  a  j=ystart
       !  a  a  a  a  a

       fld%internal%xstart = fld%grid%simulation_domain%xstart
       fld%internal%xstop  = fld%grid%simulation_domain%xstop
    else
       ! When updating a quantity on U points with this offset convention
       ! we write to (using 'x' to indicate a location that is written,
       !                    'b' a boundary point and
       !                    'o' a point that is external to the domain):
       !
       ! i=1         i=M
       !  o  b  b  b  b   j=N 
       !  o  b  x  x  b
       !  o  b  x  x  b
       !  o  b  x  x  b
       !  o  b  b  b  b   j=1
       fld%internal%xstart = fld%grid%simulation_domain%xstart + 1
       fld%internal%xstop  = fld%grid%simulation_domain%xstop
    end if

    fld%internal%ystart = fld%grid%simulation_domain%ystart
    fld%internal%ystop  = fld%grid%simulation_domain%ystop

    ! When applying periodic (wrap-around) boundary conditions (PBCs)
    ! we must fill the regions marked with 'b' above.
    ! This looks like (using x to indicate a location that is written
    ! first and y a location that is written second):
    !
    !  i=2      i=M
    ! _ y  y  y  y   j=N  
    ! /|x  o  o  o
    ! | x  o  o  o
    ! \ x  o  o  o
    !  \x_ o  o  o   j=1
    !   |\______/ 

    ! In array notation this looks like:
    !
    ! (2    , 1:N-1) = (M  , 1:N-1)
    ! (2:M, N) = (2:M, 1)

    call init_periodic_bc_halos(fld)

  end subroutine cu_sw_init

  !===================================================

  subroutine cu_ne_init(fld)
    implicit none
    class(field_type), intent(inout) :: fld

    ! Set up a field defined on U points when the grid types have 
    ! a North-East offset relative to the T point.

    ! It is the T points that define the whole domain and we are
    ! simulating a region within this domain. As a minimum, we will
    ! require one external T point around the whole perimeter of the
    ! simulated domain in order to specify boundary conditions. The U
    ! pts on the boundary will then lie between the last external and
    ! first internal T points. 
    ! With a (N)E offset this means:

    ! ji indexing: 
    ! Lowermost i index of the u points will be the same as the T's.
    ! i.e. if we start at 1 then T(1,:) are external and u(1,:) are 
    ! boundary points too.
    ! However, the U points with ji==nx will lie outside the model
    ! domain. U points with ji==nx-1 will be the Eastern-most *boundary*
    ! points.
    ! jj indexing:
    ! Lowermost j index of the U points - U pts with jj the same as
    ! external T points will also be external to domain and therefore
    ! unused. U points with jj one greater than lowest ext. T pts will
    ! be *boundary* points. U pts with jj==ny will be boundary points.

    ! When updating a quantity on U points with this offset
    ! we write to (using 'x' to indicate a location that is written and 
    ! 'b' a boundary point):
    !
    ! i= 1          nx-1  nx
    !    b   b   b   b    o   ny
    !    b   x   x   b    o 
    !    b   x   x   b    o 
    !    b   x   x   b    o   
    !    b   b   b   b    o   1
    !                         j

    ! i.e. fld(2:M,2:N+1) = ...

    if(fld%grid%boundary_conditions(1) /= BC_PERIODIC)then
      ! If we do not have periodic boundary conditions then we do
      ! not need to allow for boundary points here - they are
      ! already contained within the region defined by T mask.
      ! The T mask has been used to determine the grid%simulation_domain
      ! which describes the area on the grid that is actually being
      ! modelled (as opposed to having values supplied from B.C.'s etc.)
      fld%internal%xstart = fld%grid%simulation_domain%xstart
      fld%internal%xstop  = fld%grid%simulation_domain%xstop - 1
    else
      call gocean_stop('ERROR: cu_ne_init: implement periodic boundary conditions!')
    end if
    if(fld%grid%boundary_conditions(2) /= BC_PERIODIC)then
      fld%internal%ystart = fld%grid%simulation_domain%ystart
      fld%internal%ystop  = fld%grid%simulation_domain%ystop
    else
      call gocean_stop('ERROR: cu_ne_init: implement periodic BCs!')
    end if

!> \todo Is this concept of halo definitions useful?
    fld%num_halos = 0

  end subroutine cu_ne_init

  !===================================================

  subroutine cv_field_init(fld)
    implicit none
    class(field_type), intent(inout) :: fld

    fld%defined_on = V_POINTS

    select case(fld%grid%offset)

    case(OFFSET_SW)
       call cv_sw_init(fld)

    case(OFFSET_NE)
       call cv_ne_init(fld)

    case default
       call gocean_stop('cv_field_init: ERROR - unsupported grid offset!')

    end select

  end subroutine cv_field_init

  !===================================================

  subroutine cv_sw_init(fld)
    implicit none
    class(field_type), intent(inout) :: fld

    if(fld%grid%boundary_conditions(2) == BC_PERIODIC)then
       ! When implementing periodic boundary conditions, all
       ! mesh point types have the same extents as the grid of
       ! T points. We then have a halo of width 1 on either side
       ! of the domain.
       fld%internal%xstart = fld%grid%simulation_domain%xstart
       fld%internal%xstop  = fld%grid%simulation_domain%xstop

       fld%internal%ystart = fld%grid%simulation_domain%ystart
       fld%internal%ystop  = fld%grid%simulation_domain%ystop
    else
       ! When updating a quantity on V points we write to:
       ! (using x to indicate a location that is written):
       !
       ! i=1      i=M
       !  b  b  b  b   j=N 
       !  b  x  x  b
       !  b  x  x  b
       !  b  b  b  b
       !  o  o  o  o   j=1
       ! We're not offset from the T points in the x dimension so
       ! we have the same x bounds.
       fld%internal%xstart = fld%grid%simulation_domain%xstart
       fld%internal%xstop  = fld%grid%simulation_domain%xstop

       fld%internal%ystart = fld%grid%simulation_domain%ystart + 1
       fld%internal%ystop  = fld%grid%simulation_domain%ystop
       call gocean_stop('cv_sw_init: IMPLEMENT non-periodic BCs!')
    endif

    ! When applying periodic (wrap-around) boundary conditions (PBCs)
    ! we must fill the regions marked with 'b' above.
    ! This looks like (using x to indicate a location that is written
    ! first and y a location that is written second):
    !
    !  i=1     i=M
    !  -o  o  o  y   j=N  
    ! / o  o  o  y
    ! | o  o  o  y
    ! \ o  o  o  y
    !  \x  x  x _y   j=2
    !    \______/|

    ! In array notation this looks like:
    ! First row = last internal row
    ! field(1:M    ,1:1  ) = field(1:M,N-1:N-1)
    ! Last col = first internal col
    ! field(M:M,1:N) = field(2:2,  1:N)

    call init_periodic_bc_halos(fld)

  end subroutine cv_sw_init

  !===================================================

  subroutine cv_ne_init(fld)
    implicit none
    class(field_type), intent(inout) :: fld

    ! ji indexing:
    ! Lowermost ji index of the V points will be the same as the T's.
    ! If the domain starts at 1 then T(1,:) are external and v(1,:)
    ! are boundary points.
    ! Uppermost ji index is nx. T(nx,:) are external and v(nx,:)
    ! are boundary points.

    ! jj indexing:
    ! If domain starts at 1 then T(:,1) are external and V(:,1) are
    ! boundary points.
    ! Uppermost jj index is ny. T(ny,:) are external and so are V(ny,:)
    ! (see diagram at start of module). It is V(ny-1,:) that are the 
    ! boundary points.

    ! When updating a quantity on V points with this offset
    ! we write to (using 'x' to indicate a location that is written):
    !
    ! i=1       Nx
    !  o  o  o  o   Ny
    !  b  b  b  b   Ny-1
    !  b  x  x  b
    !  b  x  x  b
    !  b  b  b  b   j=1
    !

    if(fld%grid%boundary_conditions(1) /= BC_PERIODIC)then
      ! If we do not have periodic boundary conditions then we do
      ! not need to allow for boundary points here - they are
      ! already contained within the region.
      fld%internal%xstart = fld%grid%simulation_domain%xstart
      fld%internal%xstop  = fld%grid%simulation_domain%xstop
    else
      call gocean_stop('ERROR: cv_ne_init: implement periodic BCs!')
    end if

    if(fld%grid%boundary_conditions(2) /= BC_PERIODIC)then
      fld%internal%ystart = fld%grid%simulation_domain%ystart
      fld%internal%ystop  = fld%grid%simulation_domain%ystop - 1
    else
      call gocean_stop('ERROR: cv_ne_init: implement periodic BCs!')
    end if

  end subroutine cv_ne_init

  !===================================================

  subroutine ct_field_init(fld)
    implicit none
    class(field_type), intent(inout) :: fld

    fld%defined_on = T_POINTS

    select case(fld%grid%offset)

    case(OFFSET_SW)
       call ct_sw_init(fld)

    case(OFFSET_NE)
       call ct_ne_init(fld)

    case default
       call gocean_stop('ct_field_init: ERROR - unsupported grid offset!')

    end select

  end subroutine ct_field_init

  !===================================================

  subroutine ct_sw_init(fld)
    implicit none
    class(field_type), intent(inout) :: fld

    ! When updating a quantity on T points we write to:
    ! (using x to indicate a location that is written):
    !
    ! i=1      i=M
    !  b  b  b  b   j=N 
    !  b  x  x  b
    !  b  x  x  b
    !  b  b  b  b   j=1

    fld%internal%xstart = fld%grid%simulation_domain%xstart
    fld%internal%xstop  = fld%grid%simulation_domain%xstop
    fld%internal%ystart = fld%grid%simulation_domain%ystart
    fld%internal%ystop  = fld%grid%simulation_domain%ystop

    ! When applying periodic (wrap-around) boundary conditions
    ! (PBCs) we must fill the regions marked with 'b' above.
    ! This looks like (using x to indicate a location that is 
    ! written first and y a location that is written second):
    !
    !  i=1      i=M
    ! _ y  y  y  y   j=N  
    ! /|o  o  o  x
    ! | o  o  o  x
    ! \ o  o  o  x
    !  \o  o  o _x   j=1
    !    \______/|

    ! In array notation this looks like:
    ! Last col = first col
    ! field(M:M,  1:N-1  ) = field(1:1  ,1:N-1)
    ! Last row = first row
    ! field(1:M,N:N) = field(1:M,1:1)

    call init_periodic_bc_halos(fld)

  end subroutine ct_sw_init

  !===================================================

  subroutine ct_ne_init(fld)
    implicit none
    class(field_type), intent(inout) :: fld

    ! When updating a quantity on T points with a NE offset
    ! we write to (using x to indicate a location that is written):
    !
    ! i=1          Nx
    !  b  b  b  b  b Ny
    !  b  x  x  x  b
    !  b  x  x  x  b 
    !  b  x  x  x  b
    !  b  b  b  b  b j=1

    if(fld%grid%boundary_conditions(1) /= BC_PERIODIC)then
      ! If we do not have periodic boundary conditions then we do
      ! not need to allow for boundary points here - they are
      ! already contained within the region.
      ! Start and stop are just the same as those calculated from the T mask
      ! earlier because this is a field on T points.
      fld%internal%xstart = fld%grid%simulation_domain%xstart
      fld%internal%xstop  = fld%grid%simulation_domain%xstop
    else
      call gocean_stop('ERROR: ct_ne_init: implement periodic BCs!')
    end if

    if(fld%grid%boundary_conditions(2) /= BC_PERIODIC)then
      ! Start and stop are just the same as those calculated from the T mask
      ! earlier because this is a field on T points.
      fld%internal%ystart = fld%grid%simulation_domain%ystart
      fld%internal%ystop  = fld%grid%simulation_domain%ystop
    else
      call gocean_stop('ERROR: ct_ne_init: implement periodic BCs!')
    end if

  end subroutine ct_ne_init

  !===================================================

  subroutine cf_field_init(fld)
    implicit none
    class(field_type), intent(inout) :: fld

    fld%defined_on = F_POINTS

    select case(fld%grid%offset)

    case(OFFSET_SW)
       call cf_sw_init(fld)

    case(OFFSET_NE)
       call cf_ne_init(fld)

    case default
       call gocean_stop('cf_field_init: ERROR - unsupported grid offset!')

    end select

  end subroutine cf_field_init

  !===================================================

  subroutine cf_sw_init(fld)
    implicit none
    class(field_type), intent(inout) :: fld

    ! When updating a quantity on F points we write to:
    ! (using x to indicate a location that is written):
    !
    ! i=1         i=M
    !  o  b  b  b  b   j=N 
    !  o  b  x  x  b
    !  o  b  x  x  b
    !  o  b  b  b  b
    !  o  o  o  o  o   j=1
    if(fld%grid%boundary_conditions(1) == BC_PERIODIC)then
       fld%internal%xstart = fld%grid%simulation_domain%xstart + 1
       fld%internal%xstop  = fld%internal%xstart + fld%grid%simulation_domain%nx - 1
    else
       fld%internal%xstart = fld%grid%simulation_domain%xstart + 1
       fld%internal%xstop  = fld%grid%simulation_domain%xstop
       ! I think these are correct but we stop because I've not properly
       ! gone through the coding.
       call gocean_stop('cf_sw_init: CHECK non-periodic BCs!')
    end if

    if(fld%grid%boundary_conditions(2) == BC_PERIODIC)then
       fld%internal%ystart = fld%grid%simulation_domain%ystart + 1
       fld%internal%ystop  = fld%internal%ystart + fld%grid%simulation_domain%ny - 1
    else
       fld%internal%ystart = fld%grid%simulation_domain%ystart + 1
       fld%internal%ystop  = fld%grid%simulation_domain%ystop
       ! I think these are correct but we stop because I've not properly
       ! gone through the coding.
       call gocean_stop('cf_sw_init: CHECK non-periodic BCs!')
    end if


    ! When applying periodic (wrap-around) boundary conditions
    ! (PBCs) we must fill the regions marked with 'b' above.
    ! This looks like (using x to indicate a location that is 
    ! written first and y a location that is written second):
    !
    !  i=2      i=M
    ! .-x  o  o  o   j=N  
    ! | x  o  o  o
    ! | x  o  o  o
    ! | x  o  o  o
    ! ->y_ y  y  y   j=2
    !   |\______/ 

    ! In array notation this looks like:
    ! First col = last col
    ! field(2:2, 2:N) = field(M:M, 2:N)
    ! First row = last row
    ! field(2:M, 2:2) = field(2:M, N:N)

    call init_periodic_bc_halos(fld)

  end subroutine cf_sw_init

  !===================================================

  subroutine cf_ne_init(fld)
    implicit none
    class(field_type), intent(inout) :: fld

    ! When updating a quantity on F points we write to:
    ! (using x to indicate a location that is written
    !        b a boundary point - defined by ext. b.c.
    !        o a point that is external to the domain):
    !
    ! i=1       Nx
    !  o  o  o  o   Ny
    !  b  b  b  o   
    !  b  x  b  o   
    !  b  x  b  o
    !  b  b  b  o   j=1

    if(fld%grid%boundary_conditions(1) /= BC_PERIODIC)then
      ! If we do not have periodic boundary conditions then we do
      ! not need to allow for boundary points here - they are
      ! already contained within the region.
      fld%internal%xstart = fld%grid%simulation_domain%xstart
      fld%internal%xstop  = fld%grid%simulation_domain%xstop - 1
    else
      call gocean_stop('ERROR: cf_ne_init: implement periodic BCs!')
      stop
    end if

    if(fld%grid%boundary_conditions(2) /= BC_PERIODIC)then
      fld%internal%ystart = fld%grid%simulation_domain%ystart
      fld%internal%ystop  = fld%grid%simulation_domain%ystop - 1
    else
      call gocean_stop('ERROR: cf_ne_init: implement periodic BCs!')
    end if

  end subroutine cf_ne_init

  !===================================================

  SUBROUTINE copy_scalar_field(field_in, field_out)
    IMPLICIT none
    type(scalar_field), INTENT(in) :: field_in
    type(scalar_field), INTENT(out) :: field_out

    field_out = field_in

  END SUBROUTINE copy_scalar_field

  !===================================================

  SUBROUTINE copy_2dfield_array(field_in, field_out)
    IMPLICIT none
    REAL(wp), INTENT(in),  DIMENSION(:,:) :: field_in
    REAL(wp), INTENT(out), DIMENSION(:,:) :: field_out
        
    field_out(:,:) = field_in(:,:)
        
  end subroutine copy_2dfield_array

  !===================================================

  !> Copy from one patch in an array to another patch
  !! Will be superceded by interface using field object
  !! instead of array data.
  subroutine copy_2dfield_array_patch(field, src, dest)
    implicit none
    real(wp),          intent(inout), dimension(:,:) :: field
    type(region_type), intent(in)                    :: src, dest

    field(dest%xstart:dest%xstop,dest%ystart:dest%ystop) = &
     field(src%xstart:src%xstop ,src%ystart:src%ystop)
        
  end subroutine copy_2dfield_array_patch

  !===================================================

  SUBROUTINE copy_2dfield(field_in, field_out)
    IMPLICIT none
    type(r2d_field), intent(in)    :: field_in
    type(r2d_field), intent(inout) :: field_out
    integer :: it, ji, jj

!$OMP DO SCHEDULE(RUNTIME)
    do it = 1, field_out%ntiles, 1
       do jj= field_out%tile(it)%whole%ystart, field_out%tile(it)%whole%ystop
          do ji = field_out%tile(it)%whole%xstart, field_out%tile(it)%whole%xstop
             field_out%data(ji,jj) = field_in%data(ji,jj)
          end do
       end do
    end do
!$OMP END DO
        
  end subroutine copy_2dfield

  !===================================================

  !> Copy from one patch to another in a field
  subroutine copy_2dfield_patch(field, src, dest)
    implicit none
    type(r2d_field), intent(inout) :: field
    type(region_type),    intent(in)    :: src, dest

    field%data(dest%xstart:dest%xstop,dest%ystart:dest%ystop) = &
     field%data(src%xstart:src%xstop ,src%ystart:src%ystop)
        
  end subroutine copy_2dfield_patch

  !===================================================

  subroutine increment_scalar_field(field, incr)
    implicit none
    type(scalar_field), intent(inout) :: field
    type(scalar_field), intent(in)    :: incr

    field%data = field%data + incr%data

  END SUBROUTINE increment_scalar_field

  !===================================================

  subroutine increment_scalar_field_r8(field, incr)
    implicit none
    type(scalar_field), intent(inout) :: field
    real(wp), intent(in)    :: incr

    field%data = field%data + incr

  END SUBROUTINE increment_scalar_field_r8

  !===================================================

  SUBROUTINE set_field(fld, val)
    implicit none
    class(field_type), INTENT(out) :: fld
    real(wp), INTENT(in) :: val

    select type(fld)
    type is (scalar_field)
       fld%data = val
    type is (r2d_field)
       fld%data = val
    class default
    end select

  END SUBROUTINE set_field

  !===================================================

  !> Compute the checksum of the internal values of the supplied field
  !> object
  function fld_checksum(field) result(val)
    implicit none
    type(r2d_field), intent(in) :: field
    real(wp) :: val

    !> \todo Could add an OpenMP implementation
    val = SUM( ABS(field%data(field%internal%xstart:field%internal%xstop, &
                              field%internal%ystart:field%internal%ystop)) )

  end function fld_checksum

  !===================================================

  !> Compute the checksum of ALL of the elements of supplied array
  function array_checksum(field) result(val)
    implicit none
    real(wp), dimension(:,:), intent(in) :: field
    real(wp) :: val

    val = SUM( ABS(field(:,:) ) )

  end function array_checksum

  !===================================================

  subroutine init_periodic_bc_halos(fld)
    implicit none
    class(field_type), intent(inout) :: fld
    ! Locals
    integer :: ihalo

    ! Check whether we have PBCs in x AND y dimensions
    fld%num_halos = 0
    if( fld%grid%boundary_conditions(1) == BC_PERIODIC )then
       fld%num_halos = fld%num_halos + 2
    end if
    if( fld%grid%boundary_conditions(2) == BC_PERIODIC )then
       fld%num_halos = fld%num_halos + 2
    end if

    allocate( fld%halo(fld%num_halos) )

    ihalo = 0
    if( fld%grid%boundary_conditions(1) == BC_PERIODIC )then
       ! E-most column set to W-most internal column
       ihalo = ihalo + 1
       fld%halo(ihalo)%dest%xstart = fld%internal%xstop + 1
       fld%halo(ihalo)%dest%xstop  = fld%internal%xstop + 1
       fld%halo(ihalo)%dest%ystart = fld%internal%ystart   
       fld%halo(ihalo)%dest%ystop  = fld%internal%ystop

       fld%halo(ihalo)%source%xstart = fld%internal%xstart   
       fld%halo(ihalo)%source%xstop  = fld%internal%xstart
       fld%halo(ihalo)%source%ystart = fld%internal%ystart  
       fld%halo(ihalo)%source%ystop  = fld%internal%ystop

       ! W-most column set to E-most internal column
       ihalo = ihalo + 1
       fld%halo(ihalo)%dest%xstart = fld%internal%xstart-1   
       fld%halo(ihalo)%dest%xstop  = fld%internal%xstart-1
       fld%halo(ihalo)%dest%ystart = fld%internal%ystart   
       fld%halo(ihalo)%dest%ystop  = fld%internal%ystop

       fld%halo(ihalo)%source%xstart = fld%internal%xstop 
       fld%halo(ihalo)%source%xstop  = fld%internal%xstop
       fld%halo(ihalo)%source%ystart = fld%internal%ystart
       fld%halo(ihalo)%source%ystop  = fld%internal%ystop
    end if

    if( fld%grid%boundary_conditions(2) == BC_PERIODIC )then
       ! N-most row set to S-most internal row
       ihalo = ihalo + 1
       fld%halo(ihalo)%dest%xstart = fld%internal%xstart - 1   
       fld%halo(ihalo)%dest%xstop  = fld%internal%xstop  + 1
       fld%halo(ihalo)%dest%ystart = fld%internal%ystop+1   
       fld%halo(ihalo)%dest%ystop  = fld%internal%ystop+1

       fld%halo(ihalo)%source%xstart = fld%internal%xstart - 1
       fld%halo(ihalo)%source%xstop  = fld%internal%xstop  + 1
       fld%halo(ihalo)%source%ystart = fld%internal%ystart 
       fld%halo(ihalo)%source%ystop  = fld%internal%ystart

       ! S-most row set to N-most internal row
       ihalo = ihalo + 1
       fld%halo(ihalo)%dest%xstart = fld%internal%xstart - 1   
       fld%halo(ihalo)%dest%xstop  = fld%internal%xstop  + 1
       fld%halo(ihalo)%dest%ystart = fld%internal%ystart - 1   
       fld%halo(ihalo)%dest%ystop  = fld%internal%ystart - 1

       fld%halo(ihalo)%source%xstart = fld%internal%xstart - 1 
       fld%halo(ihalo)%source%xstop  = fld%internal%xstop  + 1
       fld%halo(ihalo)%source%ystart = fld%internal%ystop 
       fld%halo(ihalo)%source%ystop  = fld%internal%ystop
    end if

  end subroutine init_periodic_bc_halos

  !================================================

  SUBROUTINE tile_setup(fld, nx_arg, ny_arg)
    use omp_lib, only: omp_get_max_threads
    implicit none
    !> Dimensions of the model mesh
    type(r2d_field), intent(inout) :: fld
    !> Optional specification of the dimensions of the tiling grid
    integer, intent(in), optional :: nx_arg, ny_arg
    integer :: nx, ny
    INTEGER :: ival, jval ! For tile extent calculation
    integer :: internal_width, internal_height
    INTEGER :: ierr, nwidth
    INTEGER :: ji,jj, ith
    INTEGER :: nthreads       ! No. of OpenMP threads being used
    INTEGER :: jover, junder, idytmp
    INTEGER :: iover, iunder, idxtmp
    ! For doing stats on tile sizes
    INTEGER :: nvects, nvects_sum, nvects_min, nvects_max 
    LOGICAL, PARAMETER :: print_tiles = .TRUE.
    ! Whether to automatically compute the dimensions of the tiling grid
    logical :: auto_tile
    integer :: xlen, ylen
    integer :: ntilex, ntiley

    if(.not. TILED_FIELDS)then
       fld%ntiles = 1
       allocate(fld%tile(fld%ntiles), Stat=ierr)
       if(ierr /= 0 )then
          call gocean_stop('Harness: ERROR: failed to allocate tiling structures')
       end if

       ! We only have one tile and so we can simply copy in the
       ! regions already defined for the field as a whole
       fld%tile(1)%whole    = fld%whole
       fld%tile(1)%internal = fld%internal

       return
    end if

    xlen = fld%internal%nx
    ylen = fld%internal%ny

    ! Set-up regular grid of tiles
    auto_tile = .TRUE.

    ! Dimensions of the grid of tiles. 
    if(get_grid_dims(nx,ny) )then
       ntilex = nx
       ntiley = ny
       auto_tile = .FALSE.
    else if( present(nx_arg) .and. present(ny_arg) )then
       ntilex = nx_arg
       ntiley = ny_arg
       auto_tile = .FALSE.
    else
       ntilex = 1
       ntiley = 1
    end if
    
    fld%ntiles = ntilex*ntiley
    nthreads = 1
!$  nthreads = omp_get_max_threads()
    WRITE (*,"(/'Have ',I3,' OpenMP threads available.')") nthreads

    ! If we've not manually specified a grid of tiles then use the no. of
    ! threads
    IF(fld%ntiles == 1 .AND. auto_tile)fld%ntiles = nthreads

    IF(auto_tile)THEN

       ntilex = INT( SQRT(REAL(fld%ntiles)) )
       DO WHILE(MOD(fld%ntiles,ntilex) /= 0)
          ntilex = ntilex - 1
       END DO
       ntiley = fld%ntiles/ntilex

       ! Match longest dimension of MPI domain to longest dimension of 
       ! thread grid
       IF(xlen > ylen)THEN
          IF( ntilex < ntiley )THEN
             ierr   = ntiley
             ntiley = ntilex
             ntilex = ierr
          END IF
       ELSE
          ! N >= M so want nthready >= nthreadx
          IF( ntiley < ntilex )THEN
             ierr   = ntiley
             ntiley = ntilex
             ntilex = ierr
          END IF
       END IF

    END IF ! automatic determination of tiling grid

    WRITE (*,"('OpenMP thread tiling using grid of ',I3,'x',I3)") ntilex,ntiley
    write(*,*) 'ntiles for this field = ',fld%ntiles

    ALLOCATE(fld%tile(fld%ntiles), Stat=ierr)
    IF(ierr /= 0 )THEN
       call gocean_stop('Harness: ERROR: failed to allocate tiling structures')
    END IF

    ! Tiles at left and right of domain only have single
    ! overlap. Every other tile has two overlaps. So: 
    ! xlen = (ntilex-2)*(idx-2) + 2*(idx-1)
    !      = ntilex.idx - 2.ntilex - 2.idx + 4 + 2.idx - 2
    !      = ntilex.idx + 2 - 2.ntilex
    !=> idx = (xlen - 2 + 2.ntilex)/ntilex
    ! where idx is the whole width of a tile.
    !idx = NINT(REAL(xlen - 2 + 2*ntilex)/REAL(ntilex))
    !idy = NINT(REAL(ylen - 2 + 2*ntiley)/REAL(ntiley))
    ! Alternatively, if we think about the internal regions of the tiles,
    ! then they should share the domain between them:
    internal_width = NINT(REAL(xlen) / REAL(ntilex))
    internal_height = NINT(REAL(ylen) / REAL(ntiley))

    ! Integer arithmetic means that ntiley tiles of height idy might
    ! actually span a height greater or less than N. If so, we try and
    ! reduce the height of each row by just one until we've accounted
    ! for the <jover> extra rows.
    !nwidth = (ntiley-2)*(idy-2) + 2*(idy-1)
    nwidth = ntiley * internal_height
    IF(nwidth > ylen)THEN
       jover  = nwidth - ylen
       junder = 0
    ELSE IF(nwidth < ylen)THEN
       jover  = 0
       junder = ylen - nwidth
    ELSE
       jover  = 0
       junder = 0
    END IF
    ! Ditto for x dimension
    !nwidth = (ntilex-2)*(idx-2) + 2*(idx-1)
    nwidth = ntilex * internal_width
    IF(nwidth > xlen)THEN
       iover  = nwidth - xlen
       iunder = 0
    ELSE IF(nwidth < xlen)THEN
       iover  = 0
       iunder = xlen - nwidth
    ELSE
       iover  = 0
       iunder = 0
    END IF

    ! For AVX (256-bit vector) instructions, I think we want
    ! MOD(idx,4) == 0 idx = idx + (4 - MOD(idx,4))

    WRITE(*,"('Tile width = ',I4,', tile height = ',I4)") &
         internal_width, internal_height
    WRITE(*,"('iover = ',I3,', iunder = ',I3)") iover, iunder
    WRITE(*,"('jover = ',I3,', junder = ',I3)") jover, junder

    ith = 1
    ! The starting point of the tiles in y
    jval = fld%internal%ystart

    nvects_max = 0
    nvects_min = 1000000
    nvects_sum = 0
    max_tile_width  = 0
    max_tile_height = 0

    IF(print_tiles)WRITE(*,"(/'Tile dimensions:')")

    DO jj = 1, ntiley, 1

       ! If necessary, correct the height of this tile row
       IF(jover > 0)THEN
          idytmp = internal_height - 1
          jover = jover - 1
       ELSE IF(junder > 0)THEN
          idytmp = internal_height + 1
          junder = junder - 1
       ELSE
          idytmp = internal_height
       END IF

       ! The starting point of the tiles in x
       ival = fld%internal%xstart

       DO ji = 1, ntilex, 1
         
          ! If necessary, correct the width of this tile column
          IF(iover > 0)THEN
             idxtmp = internal_width - 1
             iover = iover - 1
          ELSE IF(iunder > 0)THEN
             idxtmp = internal_width + 1
             iunder = iunder - 1
          ELSE
             idxtmp = internal_width
          END IF

          if(ji == 1)then
             fld%tile(ith)%whole%xstart    = fld%whole%xstart
             fld%tile(ith)%internal%xstart = ival
          else
             fld%tile(ith)%internal%xstart = ival
             fld%tile(ith)%whole%xstart    = ival
          end if
          
          IF(ji == ntilex)THEN
             fld%tile(ith)%internal%xstop = fld%internal%xstop
             fld%tile(ith)%whole%xstop = fld%whole%xstop
          ELSE
             fld%tile(ith)%internal%xstop =  MIN(fld%internal%xstop-1, &
                                     fld%tile(ith)%internal%xstart + idxtmp - 1)
             fld%tile(ith)%whole%xstop = fld%tile(ith)%internal%xstop
          END IF
          
          if(jj == 1)then
             fld%tile(ith)%whole%ystart    = fld%whole%ystart
             fld%tile(ith)%internal%ystart = jval
          else
             fld%tile(ith)%whole%ystart    = jval
             fld%tile(ith)%internal%ystart = jval
          end if

          IF(jj /= ntiley)THEN
             fld%tile(ith)%internal%ystop =  MIN(fld%tile(ith)%internal%ystart+idytmp-1, &
                                             fld%internal%ystop-1)
             fld%tile(ith)%whole%ystop = fld%tile(ith)%internal%ystop
          ELSE
             fld%tile(ith)%internal%ystop = fld%internal%ystop
             fld%tile(ith)%whole%ystop = fld%whole%ystop
          END IF

          IF(print_tiles)THEN
             WRITE(*,"('tile[',I4,'](',I4,':',I4,')(',I4,':',I4,'), "// &
                  & "interior:(',I4,':',I4,')(',I4,':',I4,') ')")       &
                  ith,                                                  &
                  fld%tile(ith)%whole%xstart, fld%tile(ith)%whole%xstop,       &
                  fld%tile(ith)%whole%ystart, fld%tile(ith)%whole%ystop,       &
                  fld%tile(ith)%internal%xstart, fld%tile(ith)%internal%xstop, &
                  fld%tile(ith)%internal%ystart, fld%tile(ith)%internal%ystop
          END IF

          ! Collect some data on the distribution of tile sizes for 
          ! loadbalance info
          nvects = (fld%tile(ith)%internal%xstop - fld%tile(ith)%internal%xstart + 1) &
                  * (fld%tile(ith)%internal%ystop - fld%tile(ith)%internal%ystart + 1)
          nvects_sum = nvects_sum + nvects
          nvects_min = MIN(nvects_min, nvects)
          nvects_max = MAX(nvects_max, nvects)

          ! For use when allocating tile-'private' work arrays
          max_tile_width  = MAX(max_tile_width, &
                  (fld%tile(ith)%whole%xstop - fld%tile(ith)%whole%xstart + 1) )
          max_tile_height = MAX(max_tile_height, &
                  (fld%tile(ith)%whole%ystop - fld%tile(ith)%whole%ystart + 1) )

          ival = fld%tile(ith)%whole%xstop
          ith = ith + 1
       END DO
       jval = fld%tile(ith-1)%whole%ystop
    END DO

    ! Allocate tiles themselves
!!$    do ith = 1, fld%ntiles, 1
!!$       allocate(fld%tile(ith)%data(fld%tile(ith)%whole%xstop, &
!!$                                   fld%tile(ith)%whole%ystop)
!!$    end do

    ! First-touch policy
!OMP PARALLEL DO schedule(runtime), default(none), shared(fld), &
!OMP             private(ith)
!!$    do ith = 1, fld%ntiles, 1
!!$       fld%tile(ith)%data(:,:) = 0.0
!!$    end do
!OMP END PARALLEL DO

    ! Print tile-size statistics
    WRITE(*,"(/'Mean tile size = ',F8.1,' pts = ',F7.1,' KB')") &
                                 REAL(nvects_sum)/REAL(fld%ntiles), &
                                 REAL(8*nvects_sum)/REAL(fld%ntiles*1024)
    WRITE(*,"('Min,max tile size (pts) = ',I6,',',I6)") nvects_min,nvects_max
    WRITE(*,"('Tile load imbalance (%) =',F6.2)") &
                           100.0*(nvects_max-nvects_min)/REAL(nvects_min)
    WRITE (*,"('Max tile dims are ',I4,'x',I4/)") max_tile_width, &
                                                  max_tile_height

  END SUBROUTINE tile_setup

  !==============================================

  !> Routine to get the dimensions of the OpenMP tiling grid.
  !! Reads the GOCEAN_OMP_GRID environment variable which
  !! should have the format "NxM" where N is nx and M is ny.
  !! Returns false if the environment variable is not set
  !! or does not conform to this format.
  function get_grid_dims(nx, ny) result(success)
    implicit none
    integer, intent(inout) :: nx, ny
    logical :: success
    character(len=20) :: lstr
    integer :: idx, ierr

    success = .FALSE.

    call get_environment_variable(NAME='GOCEAN_OMP_GRID', VALUE=lstr, &
                                  STATUS=ierr)

    if(ierr /= 0)return

    ! We expect the string to have the format 'AxB' where A and B are
    ! integers.
    idx = index(lstr, 'x')
    if(idx == 0)then
       write (*,"(/'shallow_omp_mod::get_grid_dims: failed to parse ' &
                 &  'GOCEAN_OMP_GRID string: ',(A))") TRIM(lstr)
       write (*,"('   -  will use defaults for dimensions of tiling grid')")
       return
    endif

    read(lstr(1:idx-1),*,iostat=ierr) nx
    if(ierr /= 0)return

    read(lstr(idx+1:),*,iostat=ierr) ny
    if(ierr == 0)success = .TRUE.

  end function get_grid_dims

end module field_mod
