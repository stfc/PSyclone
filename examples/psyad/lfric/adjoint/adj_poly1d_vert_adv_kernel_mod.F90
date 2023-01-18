module adj_poly1d_vert_adv_kernel_mod
  use argument_mod, only : any_discontinuous_space_1, arg_type, cell_column, &
       gh_field, gh_integer, gh_logical, gh_read, gh_inc, &
       gh_readwrite, gh_real, gh_scalar
  use constants_mod, only : i_def, l_def, r_def
  use fs_continuity_mod, only : w2, wtheta
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_poly1d_vert_adv_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(9) = (/ &
       arg_type(GH_FIELD, GH_REAL, GH_READ, Wtheta), &
       arg_type(GH_FIELD, GH_REAL, GH_INC, W2), &
       arg_type(GH_FIELD, GH_REAL, GH_READWRITE, Wtheta), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, W2), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, Wtheta), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_DISCONTINUOUS_SPACE_1), &
       arg_type(GH_SCALAR, GH_INTEGER, GH_READ), &
       arg_type(GH_SCALAR, GH_INTEGER, GH_READ), &
       arg_type(GH_SCALAR, GH_LOGICAL, GH_READ)/)
  INTEGER :: operates_on = CELL_COLUMN
  CONTAINS
  PROCEDURE, NOPASS :: adj_poly1d_vert_adv_code
END TYPE
  private

  public :: adj_poly1d_vert_adv_code

  contains
  subroutine adj_poly1d_vert_adv_code(nlayers, advective, wind, tracer, ls_wind, ls_tracer, coeff, ndata, global_order, logspace, &
&ndf_wt, undf_wt, map_wt, ndf_w2, undf_w2, map_w2, ndf_c, undf_c, map_c)
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_wt
    integer(kind=i_def), intent(in) :: undf_wt
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: undf_w2
    integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
    integer(kind=i_def), dimension(ndf_wt), intent(in) :: map_wt
    integer(kind=i_def), intent(in) :: ndf_c
    integer(kind=i_def), intent(in) :: undf_c
    integer(kind=i_def), dimension(ndf_c), intent(in) :: map_c
    integer(kind=i_def), intent(in) :: ndata
    integer(kind=i_def), intent(in) :: global_order
    real(kind=r_def), dimension(undf_wt), intent(in) :: advective
    real(kind=r_def), dimension(undf_w2), intent(inout) :: wind
    real(kind=r_def), dimension(undf_wt), intent(inout) :: tracer
    real(kind=r_def), dimension(undf_w2), intent(in) :: ls_wind
    real(kind=r_def), dimension(undf_wt), intent(in) :: ls_tracer
    real(kind=r_def), dimension(undf_c), intent(in) :: coeff
    logical(kind=l_def), intent(in) :: logspace
    integer(kind=i_def) :: k
    integer(kind=i_def) :: kmin
    integer(kind=i_def) :: kmax
    integer(kind=i_def) :: ij
    integer(kind=i_def) :: ik
    integer(kind=i_def) :: p
    integer(kind=i_def) :: vertical_order
    integer(kind=i_def) :: use_upwind
    integer(kind=i_def) :: upwind_offset
    integer(kind=i_def) :: upwind
    INTEGER(KIND = i_def), DIMENSION(global_order + 1) :: stencil
    real(kind=r_def) :: dpdz
    real(kind=r_def) :: ls_dpdz
    real(kind=r_def), dimension(0:nlayers) :: ls_log_tracer
    real(kind=r_def) :: tmp1
    real(kind=r_def) :: tmp2
    integer(kind=i_def) :: i, itmp3

    dpdz = 0.0_r_def
    ij = map_wt(1)
    if (logspace) then
      do k = 0, nlayers, 1
        ls_log_tracer(k) = LOG(ABS(ls_tracer(ij + k)))
      enddo
    end if
    vertical_order = MIN(global_order, nlayers - 1)
    use_upwind = MOD(vertical_order, 2)
    do k = nlayers - 1, 1, -1
      do p = 0, vertical_order, 1
        tmp1 = REAL(vertical_order, r_def)
        tmp2 = 0.5 * tmp1
        itmp3 = FLOOR(tmp2)
        stencil(p + 1) = k + p - itmp3
      enddo
      upwind = INT(0.5_r_def*(1.0_r_def + SIGN(1.0_r_def,ls_wind(map_w2(5)+k))),i_def)
      upwind_offset = upwind * use_upwind
      stencil = stencil - upwind_offset
      kmin = stencil(1)
      do i = 2, vertical_order + 1, 1
        if (stencil(i) < kmin) then
          kmin = stencil(i)
        end if
      enddo
      if (kmin < 0) then
        stencil = -kmin + stencil
      end if
      kmax = stencil(1)
      do i = 2, vertical_order + 1, 1
        if (stencil(i) > kmax) then
          kmax = stencil(i)
        end if
      enddo
      kmax = kmax - nlayers
      if (kmax > 0) then
        stencil = -kmax + stencil
      end if
      ls_dpdz = 0.0_r_def
      ! ARP - calculate passive ls_dpdz first.
      if (logspace) then
        do p = 1, vertical_order + 1, 1
          ik = global_order * upwind_offset + k * ndata + p + upwind_offset + map_c(1) - 1
          ls_dpdz = ls_dpdz + coeff(ik) * ls_log_tracer(stencil(p))
        end do
      else
        do p = 1, vertical_order + 1, 1
          ik = global_order * upwind_offset + k * ndata + p + upwind_offset + map_c(1) - 1
          ls_dpdz = ls_dpdz + coeff(ik) * ls_tracer(ij + stencil(p))
        enddo
      endif
      dpdz = dpdz + advective(map_wt(1) + k) * ls_wind(k + map_w2(5))
      wind(k + map_w2(5)) = wind(k + map_w2(5)) + ls_dpdz * advective(map_wt(1) + k)
      if (logspace) then
        ls_dpdz = ls_dpdz * ls_tracer(ij + k)
        tracer(ij + k) = tracer(ij + k) + ls_dpdz * dpdz
        dpdz = dpdz * ls_tracer(ij + k)
        do p = vertical_order + 1, 1, -1
          ik = global_order * upwind_offset + k * ndata + p + upwind_offset + map_c(1) - 1
          !ls_dpdz = ls_dpdz + coeff(ik) * ls_log_tracer(stencil(p))
          tracer(ij + stencil(p)) = tracer(ij + stencil(p)) + coeff(ik) * dpdz / ls_tracer(ij + stencil(p))
        enddo
      else
        do p = vertical_order + 1, 1, -1
          ik = global_order * upwind_offset + k * ndata + p + upwind_offset + map_c(1) - 1
          !ls_dpdz = ls_dpdz + coeff(ik) * ls_tracer(ij + stencil(p))
          tracer(ij + stencil(p)) = tracer(ij + stencil(p)) + coeff(ik) * dpdz
        enddo
      end if
      dpdz = 0.0
    enddo

  end subroutine adj_poly1d_vert_adv_code

end module adj_poly1d_vert_adv_kernel_mod
