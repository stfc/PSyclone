module adj_w3_advective_update_kernel_mod
  use argument_mod, only : arg_type, cell_column, gh_field, gh_operator, gh_read, gh_real, gh_readwrite, gh_inc
  use constants_mod, only : i_def, r_def
  use fs_continuity_mod, only : w2, w3
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_w3_advective_update_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(4) = (/arg_type(GH_FIELD, GH_REAL, GH_READWRITE, W3), arg_type(GH_FIELD, GH_REAL, GH_READ, W2), &
&arg_type(GH_FIELD, GH_REAL, GH_INC, W2), arg_type(GH_OPERATOR, GH_REAL, GH_READ, W3, W3)/)
  INTEGER :: operates_on = CELL_COLUMN
  CONTAINS
  PROCEDURE, NOPASS :: adj_w3_advective_update_code
END TYPE
  private

  public :: adj_w3_advective_update_code

  contains
  subroutine adj_w3_advective_update_code(cell, nlayers, advective_increment, tracer, wind, ncell_3d, m3_inv, ndf_w3, undf_w3, &
&map_w3, ndf_w2, undf_w2, map_w2)
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in) :: undf_w2
    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
    integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
    real(kind=r_def), dimension(undf_w3), intent(inout) :: advective_increment
    real(kind=r_def), dimension(undf_w2), intent(inout) :: wind
    real(kind=r_def), dimension(undf_w2), intent(in) :: tracer
    real(kind=r_def), dimension(ndf_w3,ndf_w3,ncell_3d), intent(in) :: m3_inv
    integer(kind=i_def) :: k
    integer(kind=i_def) :: ik
    real(kind=r_def) :: u
    real(kind=r_def) :: v
    real(kind=r_def) :: w
    real(kind=r_def) :: dtdx
    real(kind=r_def) :: dtdy
    real(kind=r_def) :: dtdz

    u = 0.0_r_def
    v = 0.0_r_def
    w = 0.0_r_def
    do k = nlayers - 1, 0, -1
      dtdx = -tracer(k + map_w2(1)) + tracer(k + map_w2(3))
      dtdy = -tracer(k + map_w2(2)) + tracer(k + map_w2(4))
      dtdz = -tracer(k + map_w2(5)) + tracer(k + map_w2(6))
      ik = cell * nlayers + k - nlayers + 1
      u = u + dtdx * advective_increment(map_w3(1) + k) * m3_inv(1,1,ik)
      v = v + dtdy * advective_increment(map_w3(1) + k) * m3_inv(1,1,ik)
      w = w + dtdz * advective_increment(map_w3(1) + k) * m3_inv(1,1,ik)
      advective_increment(map_w3(1) + k) = 0.0
      wind(k + map_w2(5)) = wind(k + map_w2(5)) + 0.5 * w
      wind(k + map_w2(6)) = wind(k + map_w2(6)) + 0.5 * w
      w = 0.0
      wind(k + map_w2(2)) = wind(k + map_w2(2)) + (-0.5 * v)
      wind(k + map_w2(4)) = wind(k + map_w2(4)) - 0.5 * v
      v = 0.0
      wind(k + map_w2(1)) = wind(k + map_w2(1)) + 0.5 * u
      wind(k + map_w2(3)) = wind(k + map_w2(3)) + 0.5 * u
      u = 0.0
    enddo

  end subroutine adj_w3_advective_update_code

end module adj_w3_advective_update_kernel_mod
