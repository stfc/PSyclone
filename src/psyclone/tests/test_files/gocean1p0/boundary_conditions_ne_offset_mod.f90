module boundary_conditions_ne_offset_mod
  implicit none

  private

  public bc_ssh, bc_solid_u, bc_solid_v, bc_solid_f
  public bc_ssh_code, bc_solid_u_code
  public bc_solid_v_code, bc_solid_f_code
  public bc_flather_u, bc_flather_v
  public bc_flather_u_code, bc_flather_v_code

  !=======================================

  type, extends(kernel_type) :: bc_ssh
     type(arg), dimension(3) :: meta_args =        &
          (/ arg(READ,      I_SCALAR, POINTWISE),  &
             arg(READWRITE, CT,       POINTWISE),  &
             arg(READ,      GRID_MASK_T)           &
           /)

     !> Although this is a boundary-conditions kernel, it only
     !! acts on the internal points of the domain
     integer :: ITERATES_OVER = INTERNAL_PTS

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

  !=======================================

  type, extends(kernel_type) :: bc_solid_u
     type(arg), dimension(2) :: meta_args =  &
          (/ arg(WRITE, CU, POINTWISE),      &
             arg(READ,      GRID_MASK_T)     &
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
    procedure, nopass :: code => bc_solid_u_code
  end type bc_solid_u

  !=======================================

  type, extends(kernel_type) :: bc_solid_v
     type(arg), dimension(2) :: meta_args =  &
          (/ arg(WRITE, CV, POINTWISE),      &
             arg(READ,      GRID_MASK_T)     &
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
    procedure, nopass :: code => bc_solid_v_code
  end type bc_solid_v

  !=======================================

  type, extends(kernel_type) :: bc_solid_f
     type(arg), dimension(2) :: meta_args =  &
          (/ arg(WRITE, CF, POINTWISE),      &
             arg(READ,      GRID_MASK_T)     &
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
    procedure, nopass :: code => bc_solid_f_code
  end type bc_solid_f

  !=======================================

  type, extends(kernel_type) :: bc_flather_u
     type(arg), dimension(4) :: meta_args =  &
          (/ arg(READWRITE, CU, POINTWISE),  & ! ua
             arg(READ,      CU, POINTWISE),  & ! hu
             arg(READ,      CU, POINTWISE),  & ! sshn_u
             arg(READ,      GRID_MASK_T)     &
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
    procedure, nopass :: code => bc_flather_u_code
  end type bc_flather_u

  !=======================================

  type, extends(kernel_type) :: bc_flather_v
     type(arg), dimension(4) :: meta_args =  &
          (/ arg(READWRITE, CV, POINTWISE),  & ! va
             arg(READ,      CV, POINTWISE),  & ! hv
             arg(READ,      CV, POINTWISE),  & ! sshn_v
             arg(READ,      GRID_MASK_T)     &
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
    procedure, nopass :: code => bc_flather_v_code
  end type bc_flather_v

contains
 
  !================================================

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

  !================================================
  
  !> Kernel to apply solid boundary conditions for u-velocity
  subroutine bc_solid_u_code(ji, jj, ua, tmask)
    implicit none
    integer,                  intent(in)    :: ji, jj
    integer,  dimension(:,:), intent(in)    :: tmask
    real(wp), dimension(:,:), intent(inout) :: ua

    if(tmask(ji,jj) * tmask(ji+1,jj) == 0)then
       ua(ji,jj) = 0._wp
    end if

  end subroutine bc_solid_u_code
  
  !================================================

  !> Kernel to apply solid boundary conditions for v-velocity
  subroutine bc_solid_v_code(ji, jj, va, tmask)
    implicit none
    integer,                 intent(in)    :: ji, jj
    integer, dimension(:,:), intent(in)    :: tmask
    real(wp),dimension(:,:), intent(inout) :: va

    if(tmask(ji,jj) * tmask(ji,jj+1) == 0)then
       va(ji,jj) = 0._wp
    end if

  end subroutine bc_solid_v_code
  
  !================================================

  !> Fake kernel to apply solid boundary conditions on f points
  subroutine bc_solid_f_code(ji, jj, fa, tmask)
    implicit none
    integer,                 intent(in)    :: ji, jj
    integer, dimension(:,:), intent(in)    :: tmask
    real(wp),dimension(:,:), intent(inout) :: fa

    if(tmask(ji,jj) * tmask(ji,jj+1) == 0)then
       fa(ji,jj) = 0._wp
    end if

  end subroutine bc_solid_f_code
  
  !================================================

  !> Kernel to apply Flather condition to U
  subroutine bc_flather_u_code(ji, jj, ua, hu, sshn_u, tmask)
    implicit none
    integer,                  intent(in)    :: ji, jj
    integer,  dimension(:,:), intent(in)    :: tmask
    real(wp), dimension(:,:), intent(inout) :: ua
    real(wp), dimension(:,:), intent(in)    :: hu, sshn_u
    ! Locals
    integer  :: jiu

    !                                  Du                 Dssh
    !Flather open boundary condition [---- = sqrt(g/H) * ------]
    !                                  Dn                 Dn
    ! ua and va in du/dn should be the specified tidal forcing

    ! Check whether this point lies within the domain
    if(tmask(ji,jj) + tmask(ji+1,jj) <= -1) return

    if(tmask(ji,jj) < 0) then
       jiu = ji + 1
       ua(ji,jj) = ua(jiu,jj) + &
                   sqrt(g/hu(ji,jj)) * (sshn_u(ji,jj) - sshn_u(jiu,jj))
    else if(tmask(ji+1,jj )< 0) then
       jiu = ji - 1 
       ua(ji,jj) = ua(jiu,jj) + sqrt(g/hu(ji,jj)) * &
            (sshn_u(ji,jj) - sshn_u(jiu,jj))
    end if
  
  end subroutine bc_flather_u_code

  !================================================

  !> Kernel to apply Flather boundary condition to v component
  !! of velocity
  subroutine bc_flather_v_code(ji, jj, va, hv, sshn_v, tmask)
    implicit none
    integer,                  intent(in) :: ji, jj
    integer,  dimension(:,:), intent(in) :: tmask
    real(wp), dimension(:,:), intent(inout) :: va
    real(wp), dimension(:,:), intent(in) :: hv, sshn_v
    ! Locals
    integer  :: jiv

    ! Check whether this point is inside the simulated domain
    !\todo I could set-up a V-mask using exactly the same code structure
    !! as below. Could then apply the BC and multiply by V-mask and thus
    !! remove conditionals => get vectorisation.
    IF(tmask(ji,jj) + tmask(ji,jj+1) <= -1) return
    
    IF(tmask(ji,jj) < 0) THEN
       jiv = jj + 1
       va(ji,jj) = va(ji,jiv) + SQRT(g/hv(ji,jj)) * &
                                    (sshn_v(ji,jj) - sshn_v(ji,jiv))
    ELSE IF(tmask(ji,jj+1) < 0) THEN
       jiv = jj - 1 
       va(ji,jj) = va(ji,jiv) + SQRT(g/hv(ji,jj)) * &
                                    (sshn_v(ji,jj) - sshn_v(ji,jiv))
    END IF

  end subroutine bc_flather_v_code

  !================================================

end module boundary_conditions_ne_offset_mod
