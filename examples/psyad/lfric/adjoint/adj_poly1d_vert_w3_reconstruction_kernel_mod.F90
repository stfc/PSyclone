module adj_poly1d_vert_w3_reconstruction_kernel_mod
  use argument_mod, only : any_discontinuous_space_1, arg_type, cell_column, func_type, gh_basis, gh_field, gh_inc, gh_integer, &
&gh_logical, gh_read, gh_real, gh_scalar, reference_element_data_type, gh_readwrite
  use constants_mod, only : i_def, l_def, r_def
  use fs_continuity_mod, only : w2, w3
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_poly1d_vert_w3_reconstruction_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(8) = (/arg_type(GH_FIELD, GH_REAL, GH_INC, W2), arg_type(GH_FIELD, GH_REAL, GH_READWRITE, W3), &
&arg_type(GH_FIELD, GH_REAL, GH_READ, W2), arg_type(GH_FIELD, GH_REAL, GH_READ, W3), arg_type(GH_FIELD, GH_REAL, GH_READ, &
&ANY_DISCONTINUOUS_SPACE_1), arg_type(GH_SCALAR, GH_INTEGER, GH_READ), arg_type(GH_SCALAR, GH_INTEGER, GH_READ), &
&arg_type(GH_SCALAR, GH_LOGICAL, GH_READ)/)
  INTEGER :: operates_on = CELL_COLUMN
  CONTAINS
  PROCEDURE, NOPASS :: adj_poly1d_vert_w3_reconstruction_code
END TYPE
  private

  public :: adj_poly1d_vert_w3_reconstruction_code

  contains
  subroutine adj_poly1d_vert_w3_reconstruction_code(nlayers, reconstruction, tracer, ls_wind, ls_tracer, coeff, ndata, &
&global_order, logspace, ndf_w2, undf_w2, map_w2, ndf_w3, undf_w3, map_w3, ndf_c, undf_c, map_c)
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndata
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: undf_w2
    integer(kind=i_def), intent(in) :: ndf_c
    integer(kind=i_def), intent(in) :: undf_c
    integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
    integer(kind=i_def), dimension(ndf_c), intent(in) :: map_c
    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
    integer(kind=i_def), intent(in) :: global_order
    real(kind=r_def), dimension(undf_w2), intent(inout) :: reconstruction
    real(kind=r_def), dimension(undf_w3), intent(inout) :: tracer
    real(kind=r_def), dimension(undf_w2), intent(in) :: ls_wind
    real(kind=r_def), dimension(undf_w3), intent(in) :: ls_tracer
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
    real(kind=r_def) :: polynomial_tracer
    real(kind=r_def) :: ls_polynomial_tracer
    real(kind=r_def) :: tmp1
    real(kind=r_def) :: tmp2

    integer(kind=i_def) :: i, tmp3

    polynomial_tracer = 0.0_r_def
    vertical_order = MIN(global_order, nlayers - 1)
    use_upwind = MOD(vertical_order + 1, 2)
    ij = map_w3(1)
    do k = nlayers, 0, -1
      do p = 0, vertical_order, 1
        tmp1 = REAL(vertical_order + 1, r_def)
        tmp2 = 0.5 * tmp1
        tmp3 = FLOOR(tmp2)
        stencil(p + 1) = k + p - tmp3
      enddo
      upwind = INT(0.5_r_def * SIGN(1.0_r_def, ls_wind(k + map_w2(5))) + 0.5_r_def, i_def)
      upwind_offset = upwind * use_upwind
      if (k < nlayers) then
        stencil = stencil - upwind_offset
      end if
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
      kmax = kmax - nlayers + 1
      if (kmax > 0) then
        stencil = -kmax + stencil
      end if
      polynomial_tracer = polynomial_tracer + reconstruction(map_w2(5) + k)
      reconstruction(map_w2(5) + k) = 0.0
      if (logspace) then
        ls_polynomial_tracer = 1.0_r_def
        do p = 1, vertical_order + 1
          ik = p + upwind_offset*(global_order+1) + k*ndata + map_c(1) - 1
          ls_polynomial_tracer = ls_polynomial_tracer * abs(ls_tracer(ij + stencil(p)))**coeff(ik)
        end do
        polynomial_tracer = ls_polynomial_tracer * polynomial_tracer
        do p = vertical_order + 1, 1, -1
          ik = global_order * upwind_offset + k * ndata + p + upwind_offset + map_c(1) - 1
          !ls_polynomial_tracer = ls_polynomial_tracer * ABS(ls_tracer(ij + stencil(p))) ** coeff(ik)
          tracer(ij + stencil(p)) = tracer(ij + stencil(p)) + coeff(ik) * polynomial_tracer / ls_tracer(ij + stencil(p))
        enddo
        polynomial_tracer = 0.0
      else
        do p = vertical_order + 1, 1, -1
          ik = global_order * upwind_offset + k * ndata + p + upwind_offset + map_c(1) - 1
          tracer(ij + stencil(p)) = tracer(ij + stencil(p)) + coeff(ik) * polynomial_tracer
        enddo
        polynomial_tracer = 0.0
      end if
    enddo

  end subroutine adj_poly1d_vert_w3_reconstruction_code

end module adj_poly1d_vert_w3_reconstruction_kernel_mod
