module kernel_scalar_int
  !use kind_params_mod
  !use kernel_mod
  !use argument_mod
  !use physical_params_mod
  !use grid_mod
  !use field_mod
  implicit none

  private

  public bc_ssh
  public bc_ssh_code

  !=======================================

  type, extends(kernel_type) :: bc_ssh
     type(arg), dimension(3) :: meta_args =        &
          (/ arg(READ,      I_SCALAR, POINTWISE),  &
             arg(READWRITE, CT,       POINTWISE),  &
             arg(READ,      GRID_MASK_T)           &
           /)

     !> This is a boundary-conditions kernel and therefore
     !! acts on all points of the domain rather than just
     !! those that are internal
     integer :: ITERATES_OVER = ALL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the North and East of it.
     integer :: index_offset = OFFSET_NE

  contains
    procedure, nopass :: code => bc_ssh_code
  end type bc_ssh

contains
  
  subroutine bc_ssh_code(ji, jj, istep, ssha, tmask)
    use model_mod, only: rdt
    implicit none
    integer, intent(in)  :: ji, jj
    integer, dimension(:,:),  intent(in)    :: tmask
    integer,                  intent(in)    :: istep
    real(wp), dimension(:,:), intent(inout) :: ssha
    ! Locals
    real(wp) :: amp_tide, omega_tide, rtime

    amp_tide   = 0.2_wp
    omega_tide = 2.0_wp * 3.14159_wp / (12.42_wp * 3600._wp)
    rtime = real(istep,wp) * rdt

    if(tmask(ji,jj) <= 0) return
    IF     (tmask(ji,jj-1) < 0) THEN
       ssha(ji,jj) = amp_tide * sin(omega_tide * rtime)
    ELSE IF(tmask(ji,jj+1) < 0) THEN
       ssha(ji,jj) = amp_tide * sin(omega_tide * rtime)
    ELSE IF(tmask(ji+1,jj) < 0) THEN
       ssha(ji,jj) = amp_tide * sin(omega_tide * rtime)
    ELSE IF(tmask(ji-1,jj) < 0) THEN
       ssha(ji,jj) = amp_tide * sin(omega_tide * rtime)
    END IF

  end subroutine bc_ssh_code

end module kernel_scalar_int
