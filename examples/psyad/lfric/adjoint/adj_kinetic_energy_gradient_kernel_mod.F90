module adj_kinetic_energy_gradient_kernel_mod
  use argument_mod, only : any_discontinuous_space_3, arg_type, cell_column, func_type, gh_basis, gh_diff_basis, gh_field, gh_inc, &
&gh_quadrature_xyoz, gh_read, gh_real
  use constants_mod, only : i_def, r_def
  use fs_continuity_mod, only : w2, wchi
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_kinetic_energy_gradient_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(5) = (/arg_type(GH_FIELD, GH_REAL, GH_READ, W2), arg_type(GH_FIELD, GH_REAL, GH_INC, W2), &
&arg_type(GH_FIELD, GH_REAL, GH_READ, W2), arg_type(GH_FIELD * 3, GH_REAL, GH_READ, WCHI), arg_type(GH_FIELD, GH_REAL, GH_READ, &
&ANY_DISCONTINUOUS_SPACE_3)/)
  TYPE(func_type) :: meta_funcs(2) = (/func_type(W2, GH_BASIS, GH_DIFF_BASIS), func_type(WCHI, GH_BASIS, GH_DIFF_BASIS)/)
  INTEGER :: operates_on = CELL_COLUMN
  INTEGER :: gh_shape = GH_QUADRATURE_XYoZ
  CONTAINS
  PROCEDURE, NOPASS :: adj_kinetic_energy_gradient_code
END TYPE
  private

  public :: adj_kinetic_energy_gradient_code

  contains
  subroutine adj_kinetic_energy_gradient_code(nlayers, r_u, u, ls_u, chi_1, chi_2, chi_3, panel_id, ndf_w2, undf_w2, map_w2, &
&w2_basis, w2_diff_basis, ndf_chi, undf_chi, map_chi, chi_basis, chi_diff_basis, ndf_pid, undf_pid, map_pid, nqp_h, nqp_v, wqp_h, &
&wqp_v)
    use coordinate_jacobian_mod, only : coordinate_jacobian
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: nqp_h
    integer(kind=i_def), intent(in) :: nqp_v
    integer(kind=i_def), intent(in) :: ndf_chi
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_pid
    integer(kind=i_def), intent(in) :: undf_chi
    integer(kind=i_def), intent(in) :: undf_w2
    integer(kind=i_def), intent(in) :: undf_pid
    integer(kind=i_def), dimension(ndf_chi), intent(in) :: map_chi
    integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
    integer(kind=i_def), dimension(ndf_pid), intent(in) :: map_pid
    real(kind=r_def), dimension(3,ndf_w2,nqp_h,nqp_v), intent(in) :: w2_basis
    real(kind=r_def), dimension(1,ndf_w2,nqp_h,nqp_v), intent(in) :: w2_diff_basis
    real(kind=r_def), dimension(1,ndf_chi,nqp_h,nqp_v), intent(in) :: chi_basis
    real(kind=r_def), dimension(3,ndf_chi,nqp_h,nqp_v), intent(in) :: chi_diff_basis
    real(kind=r_def), dimension(undf_w2), intent(in) :: r_u
    real(kind=r_def), dimension(undf_w2), intent(inout) :: u
    real(kind=r_def), dimension(undf_w2), intent(in) :: ls_u
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi_1
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi_2
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi_3
    real(kind=r_def), dimension(undf_pid), intent(in) :: panel_id
    real(kind=r_def), dimension(nqp_h), intent(in) :: wqp_h
    real(kind=r_def), dimension(nqp_v), intent(in) :: wqp_v
    integer :: df
    integer :: k
    integer :: loc
    integer :: ipanel
    integer :: qp1
    integer :: qp2
    real(kind=r_def), dimension(ndf_chi) :: chi_1_e
    real(kind=r_def), dimension(ndf_chi) :: chi_2_e
    real(kind=r_def), dimension(ndf_chi) :: chi_3_e
    real(kind=r_def), dimension(nqp_h,nqp_v) :: dj
    real(kind=r_def), dimension(3,3,nqp_h,nqp_v) :: jac
    real(kind=r_def), dimension(ndf_w2) :: ru_e
    real(kind=r_def), dimension(ndf_w2) :: u_e
    real(kind=r_def), dimension(ndf_w2) :: ls_u_e
    real(kind=r_def), dimension(3) :: ls_u_at_quad
    real(kind=r_def), dimension(3) :: u_at_quad
    real(kind=r_def) :: ke_at_quad
    real(kind=r_def) :: dv
    real(kind=r_def), dimension(3) :: mul1
    real(kind=r_def), dimension(3) :: mul2
    integer :: i
    real(kind=r_def) :: res_dot_product
    integer :: idx
    integer :: j
    integer :: i_1
    integer :: j_1
    integer :: idx_1

    ru_e = 0.0_r_def
    ke_at_quad = 0.0_r_def
    res_dot_product = 0.0_r_def
    mul2 = 0.0_r_def
    u_at_quad = 0.0_r_def
    u_e = 0.0_r_def
    ipanel = INT(panel_id(map_pid(1)), i_def)
    do k = nlayers - 1, 0, -1
      do df = 1, ndf_chi, 1
        loc = k + map_chi(df)
        chi_1_e(df) = chi_1(loc)
        chi_2_e(df) = chi_2(loc)
        chi_3_e(df) = chi_3(loc)
      enddo
      call coordinate_jacobian(ndf_chi, nqp_h, nqp_v, chi_1_e(:), chi_2_e(:), chi_3_e(:), ipanel, chi_basis(:,:,:,:), &
&chi_diff_basis(:,:,:,:), jac(:,:,:,:), dj(:,:))
      do df = 1, ndf_w2, 1
        ls_u_e(df) = ls_u(map_w2(df) + k)
      enddo
      do df = ndf_w2, 1, -1
        ru_e(df) = ru_e(df) + r_u(map_w2(df) + k)
      enddo
      do qp2 = nqp_v, 1, -1
        do qp1 = nqp_h, 1, -1
          ls_u_at_quad(:) = 0.0_r_def
          do df = 1, ndf_w2, 1
            ls_u_at_quad(:) = ls_u_at_quad(:) + ls_u_e(df) * w2_basis(:,df,qp1,qp2)
          enddo
          do i = 1, 3, 1
            mul1(i) = 0.0
            do j = 1, 3, 1
              mul1(i) = jac(i,j,qp1,qp2) * ls_u_at_quad(j) + mul1(i)
            enddo
          enddo
          do df = ndf_w2, 1, -1
            dv = w2_diff_basis(1,df,qp1,qp2)
            ke_at_quad = ke_at_quad + dv * ru_e(df) * wqp_h(qp1) * wqp_v(qp2)
          enddo
          res_dot_product = res_dot_product + ke_at_quad / dj(qp1,qp2) ** 2
          ke_at_quad = 0.0
          do i = 3, 1, -1
            mul2(i) = mul2(i) + mul1(i) * res_dot_product
          enddo
          res_dot_product = 0.0
          do i_1 = 3, 1, -1
            do j_1 = 3, 1, -1
              u_at_quad(j_1) = u_at_quad(j_1) + jac(i_1,j_1,qp1,qp2) * mul2(i_1)
            enddo
            mul2(i_1) = 0.0
          enddo
          do df = ndf_w2, 1, -1
            do idx_1 = UBOUND(u_at_quad, 1), LBOUND(u_at_quad, 1), -1
              u_e(df) = u_e(df) + u_at_quad(idx_1) * w2_basis(idx_1,df,qp1,qp2)
            enddo
          enddo
          do idx = UBOUND(u_at_quad, 1), LBOUND(u_at_quad, 1), -1
            u_at_quad(idx) = 0.0
          enddo
        enddo
      enddo
      do df = ndf_w2, 1, -1
        ru_e(df) = 0.0
      enddo
      do df = ndf_w2, 1, -1
        u(map_w2(df) + k) = u(map_w2(df) + k) + u_e(df)
        u_e(df) = 0.0
      enddo
    enddo

  end subroutine adj_kinetic_energy_gradient_code

end module adj_kinetic_energy_gradient_kernel_mod
