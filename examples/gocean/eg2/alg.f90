program alg
  use kind_params_mod, only: wp
  use grid_mod
  use field_mod, only: r2d_field, T_POINTS
  use inc_field_mod, only: inc_field
  implicit none
  integer :: istp, ierr, this_step
  integer :: jpiglo, jpjglo
  integer, parameter :: nsteps = 10
  
  integer, allocatable, dimension(:,:) :: tmask
  type(r2d_field) :: fld1
  type(grid_type), target :: grid1
  integer :: nx, ny
  
  ! Dimensions of our domain
  jpiglo = 50
  jpjglo = 50

  ! Create our grid
  grid1 = grid_type(ARAKAWA_C,                           &
                   (/BC_PERIODIC,BC_PERIODIC,BC_NONE/), &
                   OFFSET_SW)
  ! Set-up the T mask. This defines the model domain.
  allocate(tmask(jpiglo,jpjglo), stat=ierr)
  if(ierr /= 0)then
     stop 'Failed to allocate T mask'
  end if

  tmask(:,:) = 0

  ! Having specified the T points mask, we can set up mesh parameters
  call grid_init(grid1, jpiglo, jpjglo, 1000.0_wp, 1000.0_wp, tmask)
  
  ! Create fields on this grid
  fld1 = r2d_field(grid1, T_POINTS)

  fld1%data(:,:) = 0.0_wp

  nx = fld1%whole%nx
  ny = fld1%whole%ny
  do istp = 1, nsteps
     this_step = istp ! Workaround the fact that PSyclone declares all arguments
                      ! to the PSy-layer as INOUT and we can't do that for a
                      ! loop variable
     call invoke( inc_field(fld1, nx, ny, this_step) )
  end do

  ! Manually pull the data back from the GPU and do some "IO"
  !$acc update self(fld1%data)
  write (*,*) "nsteps = ", nsteps, "field(2,2) = ", fld1%data(2,2)

end program alg
