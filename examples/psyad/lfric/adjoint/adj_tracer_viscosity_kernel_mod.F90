module adj_tracer_viscosity_kernel_mod
  use argument_mod, only : arg_type, cell_column, cross, gh_field, gh_read, gh_real, gh_scalar, gh_readwrite, stencil
  use constants_mod, only : i_def, r_def
  use fs_continuity_mod, only : w2, wtheta
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_tracer_viscosity_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(4) = (/arg_type(GH_FIELD, GH_REAL, GH_READWRITE, Wtheta), arg_type(GH_FIELD, GH_REAL, GH_READ, Wtheta, &
&STENCIL(CROSS)), arg_type(GH_FIELD, GH_REAL, GH_READ, W2), arg_type(GH_SCALAR, GH_REAL, GH_READ)/)
  INTEGER :: operates_on = CELL_COLUMN
  CONTAINS
  PROCEDURE, NOPASS :: adj_tracer_viscosity_code
END TYPE
  private

  public :: adj_tracer_viscosity_code

  contains
  subroutine adj_tracer_viscosity_code(nlayers, theta_inc, theta_n, map_wt_size, map_wt, dx_at_w2, viscosity_mu, ndf_wt, undf_wt, &
&cell_map_wt, ndf_w2, undf_w2, map_w2)
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_wt
    integer(kind=i_def), intent(in) :: undf_wt
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: undf_w2
    integer(kind=i_def), intent(in) :: map_wt_size
    integer(kind=i_def), dimension(ndf_wt,map_wt_size), intent(in) :: map_wt
    integer(kind=i_def), dimension(ndf_wt), intent(in) :: cell_map_wt
    integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
    real(kind=r_def), dimension(undf_wt), intent(inout) :: theta_inc
    real(kind=r_def), dimension(undf_wt), intent(in) :: theta_n
    real(kind=r_def), dimension(undf_w2), intent(in) :: dx_at_w2
    real(kind=r_def), intent(inout) :: viscosity_mu
    integer(kind=i_def) :: k
    integer(kind=i_def) :: km
    integer(kind=i_def) :: kp
    real(kind=r_def) :: d2dx
    real(kind=r_def) :: d2dy
    real(kind=r_def) :: d2dz
    REAL(KIND = r_def), DIMENSION(0 : nlayers - 1) :: idx2
    REAL(KIND = r_def), DIMENSION(0 : nlayers - 1) :: idy2
    REAL(KIND = r_def), DIMENSION(0 : nlayers - 1) :: idz2

    do k = 0, nlayers - 1, 1
      idx2(k) = 4.0 / (dx_at_w2(k + map_w2(1)) ** 2 + 2 * dx_at_w2(k + map_w2(1)) * dx_at_w2(k + map_w2(3)) + dx_at_w2(k + &
&map_w2(3)) ** 2)
      idy2(k) = 4.0 / (dx_at_w2(k + map_w2(2)) ** 2 + 2 * dx_at_w2(k + map_w2(2)) * dx_at_w2(k + map_w2(4)) + dx_at_w2(k + &
&map_w2(4)) ** 2)
      idz2(k) = 1.0 / dx_at_w2(k + map_w2(5)) ** 2
    enddo
    k = 0
    km = 0
    kp = k + 1
    d2dx = -2.0 * idx2(k) * theta_n(k + map_wt(1,1)) + idx2(k) * theta_n(k + map_wt(1,2)) + idx2(k) * theta_n(k + map_wt(1,4))
    d2dy = -2.0 * idy2(k) * theta_n(k + map_wt(1,1)) + idy2(k) * theta_n(k + map_wt(1,3)) + idy2(k) * theta_n(k + map_wt(1,5))
    d2dz = -2.0 * idz2(k) * theta_n(k + map_wt(1,1)) + idz2(k) * theta_n(km + map_wt(1,1)) + idz2(k) * theta_n(kp + map_wt(1,1))
    k = nlayers
    km = k - 1
    kp = k
    d2dx = -2.0 * idx2(nlayers - 1) * theta_n(k + map_wt(1,1)) + idx2(nlayers - 1) * theta_n(k + map_wt(1,2)) + idx2(nlayers - 1) &
&* theta_n(k + map_wt(1,4))
    d2dy = -2.0 * idy2(nlayers - 1) * theta_n(k + map_wt(1,1)) + idy2(nlayers - 1) * theta_n(k + map_wt(1,3)) + idy2(nlayers - 1) &
&* theta_n(k + map_wt(1,5))
    d2dz = -2.0 * idz2(nlayers - 1) * theta_n(k + map_wt(1,1)) + idz2(nlayers - 1) * theta_n(km + map_wt(1,1)) + idz2(nlayers - 1) &
&* theta_n(kp + map_wt(1,1))
    viscosity_mu = viscosity_mu + d2dx * theta_inc(cell_map_wt(1) + k)
    viscosity_mu = viscosity_mu + d2dy * theta_inc(cell_map_wt(1) + k)
    viscosity_mu = viscosity_mu + d2dz * theta_inc(cell_map_wt(1) + k)
    theta_inc(cell_map_wt(1) + k) = 0.0
    do k = nlayers - 1, 1, -1
      km = k - 1
      kp = k + 1
      d2dx = -2.0 * idx2(k) * theta_n(k + map_wt(1,1)) + idx2(k) * theta_n(k + map_wt(1,2)) + idx2(k) * theta_n(k + map_wt(1,4))
      d2dy = -2.0 * idy2(k) * theta_n(k + map_wt(1,1)) + idy2(k) * theta_n(k + map_wt(1,3)) + idy2(k) * theta_n(k + map_wt(1,5))
      d2dz = -2.0 * idz2(k) * theta_n(k + map_wt(1,1)) + idz2(k) * theta_n(km + map_wt(1,1)) + idz2(k) * theta_n(kp + map_wt(1,1))
      viscosity_mu = viscosity_mu + d2dx * theta_inc(cell_map_wt(1) + k)
      viscosity_mu = viscosity_mu + d2dy * theta_inc(cell_map_wt(1) + k)
      viscosity_mu = viscosity_mu + d2dz * theta_inc(cell_map_wt(1) + k)
      theta_inc(cell_map_wt(1) + k) = 0.0
    enddo
    viscosity_mu = viscosity_mu + d2dx * theta_inc(cell_map_wt(1) + k)
    viscosity_mu = viscosity_mu + d2dy * theta_inc(cell_map_wt(1) + k)
    viscosity_mu = viscosity_mu + d2dz * theta_inc(cell_map_wt(1) + k)
    theta_inc(cell_map_wt(1) + k) = 0.0
    if (map_wt_size < 5_i_def) then
      return
      do k = nlayers, 0, -1
        theta_inc(cell_map_wt(1) + k) = 0.0
      enddo
    end if

  end subroutine adj_tracer_viscosity_code

end module adj_tracer_viscosity_kernel_mod
