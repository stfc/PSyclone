module continuity_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod
  use field_mod
  implicit none

  type, extends(kernel_type) :: continuity
     type(arg), dimension(10) :: meta_args =         &
          (/ arg(WRITE, CT,       POINTWISE),        & ! ssha
             arg(READ,  CT,       POINTWISE),        & ! sshn
             arg(READ,  CU,       POINTWISE),        & ! sshn_u
             arg(READ,  CV,       POINTWISE),        & ! sshn_v
             arg(READ,  CU,       POINTWISE),        & ! hu
             arg(READ,  CV,       POINTWISE),        & ! hv
             arg(READ,  CU,       POINTWISE),        & ! un
             arg(READ,  CV,       POINTWISE),        & ! vn
             arg(READ,  R_SCALAR, POINTWISE),        & ! Time-step
             arg(READ,  GRID_AREA_T)                 &
           /)

     integer :: ITERATES_OVER = INTERNAL_PTS
     integer :: index_offset = OFFSET_NE

  contains
    procedure, nopass :: code => continuity_code
  end type continuity

contains

  subroutine continuity_code(ji, jj,                     &
                             ssha, sshn, sshn_u, sshn_v, &
                             hu, hv, un, vn, rdt, e12t)
    implicit none
    integer,                  intent(in)  :: ji, jj
    real(wp),                 intent(in)  :: rdt
    real(wp), dimension(:,:), intent(in)  :: e12t
    real(wp), dimension(:,:), intent(out) :: ssha
    real(wp), dimension(:,:), intent(in)  :: sshn, sshn_u, sshn_v, &
                                             hu, hv, un, vn
    ssha(ji,jj) = 0.0_wp

  end subroutine continuity_code

end module continuity_mod
