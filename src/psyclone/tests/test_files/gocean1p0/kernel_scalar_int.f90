module kernel_scalar_int
  use argument_mod
  use field_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod
  implicit none

  private

  public bc_ssh
  public bc_ssh_code

  !=======================================

  type, extends(kernel_type) :: bc_ssh
     type(go_arg), dimension(3) :: meta_args =        &
          (/ go_arg(GO_READ,      GO_I_SCALAR, GO_POINTWISE),  &
             go_arg(GO_READWRITE, GO_CT,       GO_POINTWISE),  &
             go_arg(GO_READ,      GO_GRID_MASK_T)              &
           /)

     !> This is a boundary-conditions kernel and therefore
     !! acts on all points of the domain rather than just
     !! those that are internal
     integer :: ITERATES_OVER = GO_ALL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the North and East of it.
     integer :: index_offset = GO_OFFSET_NE

  contains
    procedure, nopass :: code => bc_ssh_code
  end type bc_ssh

contains
  
  subroutine bc_ssh_code(ji, jj, istep, ssha, tmask)
    ! This use statement is not necessary, it is only added 
    ! to test that use statements are preserved, see test
    ! gocean1p0_transformation.test_module_inline_with_sub_use
    use grid_mod
    implicit none
    integer, intent(in)  :: ji, jj
    integer, dimension(:,:),  intent(in)    :: tmask
    integer,                  intent(in)    :: istep
    real(go_wp), dimension(:,:), intent(inout) :: ssha
    ! Locals
    real(go_wp) :: amp_tide, omega_tide, rtime

    amp_tide   = 0.2_go_wp
    omega_tide = 2.0_go_wp * 3.14159_go_wp / (12.42_go_wp * 3600._go_wp)
    ! TODO: Issue #375, kinds (e.g: go_wp) is not supported in PSyIR
    ! rtime = real(istep,go_wp) * 2.0_go_wp
    rtime = real(istep) * 2.0_go_wp

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
