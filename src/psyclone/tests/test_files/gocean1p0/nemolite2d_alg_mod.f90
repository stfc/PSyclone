module nemolite2d_alg_mod

contains

subroutine step(istp,           &
                ua, va, un, vn, &
                sshn_t, sshn_u, sshn_v, &
                ssha_t, ssha_u, ssha_v, &
                hu, hv, ht)
  use kind_params_mod
  use grid_mod
  use field_mod
  use model_mod, only: rdt ! The model time-step

  use continuity_mod,  only: continuity
  use momentum_mod,    only: momentum_u
  implicit none
  !> The current time step
  integer,         intent(inout) :: istp
  type(r2d_field), intent(inout) :: un, vn, sshn_t, sshn_u, sshn_v
  type(r2d_field), intent(inout) :: ua, va, ssha_t, ssha_u, ssha_v
  type(r2d_field), intent(inout) :: hu, hv, ht

  call invoke(                                               &
              continuity(ssha_t, sshn_t, sshn_u, sshn_v,     &
                         hu, hv, un, vn, rdt),               &
              momentum_u(ua, un, vn, hu, hv, ht,             &
                         ssha_u, sshn_t, sshn_u, sshn_v)     &
             )

end subroutine step

end module nemolite2d_alg_mod
