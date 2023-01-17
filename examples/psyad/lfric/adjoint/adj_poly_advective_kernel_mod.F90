module adj_poly_advective_kernel_mod
  use argument_mod, only : arg_type, cell_column, gh_field, gh_read, gh_real, gh_readwrite, gh_inc
  use constants_mod, only : i_def, r_def
  use fs_continuity_mod, only : w1, w2, wtheta
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_poly_advective_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(5) = (/arg_type(GH_FIELD, GH_REAL, GH_READWRITE, Wtheta), arg_type(GH_FIELD, GH_REAL, GH_INC, W2), &
&arg_type(GH_FIELD, GH_REAL, GH_INC, W1), arg_type(GH_FIELD, GH_REAL, GH_READ, W2), arg_type(GH_FIELD, GH_REAL, GH_READ, W1)/)
  INTEGER :: operates_on = CELL_COLUMN
  CONTAINS
  PROCEDURE, NOPASS :: adj_poly_advective_code
END TYPE
  private

  public :: adj_poly_advective_code

  contains
  subroutine adj_poly_advective_code(nlayers, advective, wind, tracer, ls_wind, ls_tracer, ndf_wt, undf_wt, map_wt, ndf_w2, &
&undf_w2, map_w2, ndf_w1, undf_w1, map_w1)
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_wt
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: undf_wt
    integer(kind=i_def), intent(in) :: undf_w2
    integer(kind=i_def), intent(in) :: undf_w1
    integer(kind=i_def), dimension(ndf_wt), intent(in) :: map_wt
    integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
    integer(kind=i_def), dimension(ndf_w1), intent(in) :: map_w1
    real(kind=r_def), dimension(undf_wt), intent(inout) :: advective
    real(kind=r_def), dimension(undf_w2), intent(inout) :: wind
    real(kind=r_def), dimension(undf_w1), intent(inout) :: tracer
    real(kind=r_def), dimension(undf_w2), intent(in) :: ls_wind
    real(kind=r_def), dimension(undf_w1), intent(in) :: ls_tracer
    integer(kind=i_def) :: k
    real(kind=r_def) :: u
    real(kind=r_def) :: v
    real(kind=r_def) :: dtdx
    real(kind=r_def) :: dtdy
    real(kind=r_def) :: ls_u
    real(kind=r_def) :: ls_v
    real(kind=r_def) :: ls_dtdx
    real(kind=r_def) :: ls_dtdy
    real(kind=r_def) :: ls_dtdx_1
    real(kind=r_def) :: ls_dtdy_1
    real(kind=r_def) :: ls_dtdx_2
    real(kind=r_def) :: ls_dtdy_2
    real(kind=r_def) :: ls_u_1
    real(kind=r_def) :: ls_v_1
    real(kind=r_def) :: ls_u_2
    real(kind=r_def) :: ls_v_2

    dtdy = 0.0_r_def
    dtdx = 0.0_r_def
    u = 0.0_r_def
    v = 0.0_r_def
    ls_u = 0.25 * ls_wind(map_w2(1)) + 0.25 * ls_wind(map_w2(3))
    ls_v = -0.25 * ls_wind(map_w2(2)) - 0.25 * ls_wind(map_w2(4))
    ls_dtdx = -ls_tracer(map_w1(1)) + ls_tracer(map_w1(3))
    ls_dtdy = -ls_tracer(map_w1(2)) + ls_tracer(map_w1(4))
    k = nlayers - 1
    ls_u_1 = 0.25 * ls_wind(k + map_w2(1)) + 0.25 * ls_wind(k + map_w2(3))
    ls_v_1 = -0.25 * ls_wind(k + map_w2(2)) - 0.25 * ls_wind(k + map_w2(4))
    ls_dtdx_1 = -ls_tracer(k + map_w1(9)) + ls_tracer(k + map_w1(11))
    ls_dtdy_1 = -ls_tracer(k + map_w1(10)) + ls_tracer(k + map_w1(12))
    dtdx = dtdx + advective(map_wt(2) + k) * ls_u_1
    dtdy = dtdy + advective(map_wt(2) + k) * ls_v_1
    u = u + ls_dtdx_1 * advective(map_wt(2) + k)
    v = v + ls_dtdy_1 * advective(map_wt(2) + k)
    advective(map_wt(2) + k) = 0.0
    tracer(k + map_w1(10)) = tracer(k + map_w1(10)) + (-dtdy)
    tracer(k + map_w1(12)) = tracer(k + map_w1(12)) + dtdy
    dtdy = 0.0
    tracer(k + map_w1(9)) = tracer(k + map_w1(9)) + (-dtdx)
    tracer(k + map_w1(11)) = tracer(k + map_w1(11)) + dtdx
    dtdx = 0.0
    wind(k + map_w2(2)) = wind(k + map_w2(2)) + (-0.25 * v)
    wind(k + map_w2(4)) = wind(k + map_w2(4)) - 0.25 * v
    v = 0.0
    wind(k + map_w2(1)) = wind(k + map_w2(1)) + 0.25 * u
    wind(k + map_w2(3)) = wind(k + map_w2(3)) + 0.25 * u
    u = 0.0
    do k = nlayers - 1, 1, -1
      ls_u_2 = 0.25 * ls_wind(k + map_w2(1)) + 0.25 * ls_wind(k + map_w2(3)) + 0.25 * ls_wind(k + map_w2(1) - 1) + 0.25 * &
&ls_wind(k + map_w2(3) - 1)
      ls_v_2 = -0.25 * ls_wind(k + map_w2(2)) - 0.25 * ls_wind(k + map_w2(4)) - 0.25 * ls_wind(k + map_w2(2) - 1) - 0.25 * &
&ls_wind(k + map_w2(4) - 1)
      ls_dtdx_2 = -ls_tracer(k + map_w1(1)) + ls_tracer(k + map_w1(3))
      ls_dtdy_2 = -ls_tracer(k + map_w1(2)) + ls_tracer(k + map_w1(4))
      dtdx = dtdx + advective(map_wt(1) + k) * ls_u_2
      dtdy = dtdy + advective(map_wt(1) + k) * ls_v_2
      u = u + ls_dtdx_2 * advective(map_wt(1) + k)
      v = v + ls_dtdy_2 * advective(map_wt(1) + k)
      advective(map_wt(1) + k) = 0.0
      tracer(k + map_w1(2)) = tracer(k + map_w1(2)) + (-dtdy)
      tracer(k + map_w1(4)) = tracer(k + map_w1(4)) + dtdy
      dtdy = 0.0
      tracer(k + map_w1(1)) = tracer(k + map_w1(1)) + (-dtdx)
      tracer(k + map_w1(3)) = tracer(k + map_w1(3)) + dtdx
      dtdx = 0.0
      wind(k + map_w2(2)) = wind(k + map_w2(2)) + (-0.25 * v)
      wind(k + map_w2(4)) = wind(k + map_w2(4)) - 0.25 * v
      wind(k + map_w2(2) - 1) = wind(k + map_w2(2) - 1) - 0.25 * v
      wind(k + map_w2(4) - 1) = wind(k + map_w2(4) - 1) - 0.25 * v
      v = 0.0
      wind(k + map_w2(1)) = wind(k + map_w2(1)) + 0.25 * u
      wind(k + map_w2(3)) = wind(k + map_w2(3)) + 0.25 * u
      wind(k + map_w2(1) - 1) = wind(k + map_w2(1) - 1) + 0.25 * u
      wind(k + map_w2(3) - 1) = wind(k + map_w2(3) - 1) + 0.25 * u
      u = 0.0
    enddo
    dtdx = dtdx + advective(map_wt(1)) * ls_u
    dtdy = dtdy + advective(map_wt(1)) * ls_v
    u = u + ls_dtdx * advective(map_wt(1))
    v = v + ls_dtdy * advective(map_wt(1))
    advective(map_wt(1)) = 0.0
    tracer(map_w1(2)) = tracer(map_w1(2)) + (-dtdy)
    tracer(map_w1(4)) = tracer(map_w1(4)) + dtdy
    dtdy = 0.0
    tracer(map_w1(1)) = tracer(map_w1(1)) + (-dtdx)
    tracer(map_w1(3)) = tracer(map_w1(3)) + dtdx
    dtdx = 0.0
    wind(map_w2(2)) = wind(map_w2(2)) + (-0.25 * v)
    wind(map_w2(4)) = wind(map_w2(4)) - 0.25 * v
    v = 0.0
    wind(map_w2(1)) = wind(map_w2(1)) + 0.25 * u
    wind(map_w2(3)) = wind(map_w2(3)) + 0.25 * u
    u = 0.0

  end subroutine adj_poly_advective_code

end module adj_poly_advective_kernel_mod
