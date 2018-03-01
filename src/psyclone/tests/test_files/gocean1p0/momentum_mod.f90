module momentum_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod
  use field_mod
  implicit none

  private

  public momentum_u
  public momentum_u_code

  !=======================================

  type, extends(kernel_type) :: momentum_u
     type(arg), dimension(18) :: meta_args =  &
          (/ arg(READWRITE, CU, POINTWISE),  & ! ua
             arg(READ,      CU, POINTWISE),  & ! un
             arg(READ,      CV, POINTWISE),  & ! vn
             arg(READ,      CU, POINTWISE),  & ! hu
             arg(READ,      CV, POINTWISE),  & ! hv
             arg(READ,      CT, POINTWISE),  & ! ht
             arg(READ,      CU, POINTWISE),  & ! ssha_u
             arg(READ,      CT, POINTWISE),  & ! sshn_t
             arg(READ,      CU, POINTWISE),  & ! sshn_u
             arg(READ,      CV, POINTWISE),  & ! sshn_v
             arg(READ,      GRID_MASK_T),    &
             arg(READ,      GRID_DX_U),      &
             arg(READ,      GRID_DX_V),      &
             arg(READ,      GRID_DX_T),      &
             arg(READ,      GRID_DY_U),      &
             arg(READ,      GRID_DY_T),      &
             arg(READ,      GRID_AREA_U),    &
             arg(READ,      GRID_LAT_U)      &
           /)

     integer :: ITERATES_OVER = INTERNAL_PTS
     integer :: index_offset = OFFSET_NE

  contains
    procedure, nopass :: code => momentum_u_code
  end type momentum_u

contains

  subroutine momentum_u_code(ji, jj, &
                             ua, un, vn, &
                             hu, hv, ht, ssha_u, &
                             sshn, sshn_u, sshn_v, &
                             tmask, e1u, e1v, e1t, e2u, e2t, e12u, gphiu)
    use model_mod, only: rdt, cbfr, visc
    implicit none
    integer, intent(in) :: ji, jj
    integer,  dimension(:,:), intent(in) :: tmask
    real(wp), dimension(:,:), intent(in) :: e1u, e1v, e1t, e12u, e2u, e2t, gphiu
    real(wp), dimension(:,:), intent(in) :: hu, hv, ht
    real(wp), dimension(:,:), intent(in) :: ssha_u, sshn, sshn_u, sshn_v
    real(wp), dimension(:,:), intent(in) :: un, vn
    real(wp), dimension(:,:), intent(out) :: ua

    !kernel ua calculation 
    ua(ji,jj) = 0.0_wp

  end subroutine momentum_u_code 

end module momentum_mod
