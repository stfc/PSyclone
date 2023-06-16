module adj_vorticity_advection_kernel_mod
  use kernel_mod, only : kernel_type
  use argument_mod, only : any_discontinuous_space_3, arg_type, cell_column, func_type, gh_basis, gh_diff_basis, &
&gh_field, gh_inc, gh_quadrature_xyoz, gh_read, gh_real
  use constants_mod, only : i_def, r_def
  use fs_continuity_mod, only : w1, w2, Wchi
  use cross_product_mod, only : cross_product
  implicit none
  type, public, extends(kernel_type) :: adj_vorticity_advection_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(7) = (/ &
       arg_type(GH_FIELD, GH_REAL, GH_READ, W2), &
       arg_type(GH_FIELD, GH_REAL, GH_INC, W2), &
       &arg_type(GH_FIELD, GH_REAL, GH_INC, W1), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, W2), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, W1), &
       &arg_type(GH_FIELD * 3, GH_REAL, GH_READ, Wchi), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_DISCONTINUOUS_SPACE_3)/)
  TYPE(func_type) :: meta_funcs(3) = (/ &
       func_type(W2, GH_BASIS), &
       func_type(W1, GH_BASIS), &
       func_type(Wchi, GH_BASIS, GH_DIFF_BASIS)/)
  INTEGER :: operates_on = CELL_COLUMN
  INTEGER :: gh_shape = GH_QUADRATURE_XYoZ
  CONTAINS
  PROCEDURE, NOPASS :: adj_vorticity_advection_code
END TYPE
  public

  public :: adj_vorticity_advection_code

  contains
  subroutine adj_vorticity_advection_code(nlayers, r_u, wind, vorticity, ls_wind, ls_vorticity, chi_1, chi_2, chi_3, panel_id, &
&ndf_w2, undf_w2, map_w2, w2_basis, ndf_w1, undf_w1, map_w1, w1_basis, ndf_chi, undf_chi, map_chi, chi_basis, chi_diff_basis, &
&ndf_pid, undf_pid, map_pid, nqp_h, nqp_v, wqp_h, wqp_v)
    use coordinate_jacobian_mod, only : pointwise_coordinate_jacobian, pointwise_coordinate_jacobian_inverse
    integer, intent(in) :: nlayers
    integer, intent(in) :: nqp_h
    integer, intent(in) :: nqp_v
    integer, intent(in) :: ndf_chi
    integer, intent(in) :: ndf_w1
    integer, intent(in) :: ndf_w2
    integer, intent(in) :: ndf_pid
    integer, intent(in) :: undf_chi
    integer, intent(in) :: undf_w1
    integer, intent(in) :: undf_w2
    integer, intent(in) :: undf_pid
    integer, dimension(ndf_chi), intent(in) :: map_chi
    integer, dimension(ndf_w1), intent(in) :: map_w1
    integer, dimension(ndf_w2), intent(in) :: map_w2
    integer, dimension(ndf_pid), intent(in) :: map_pid
    real(kind=r_def), dimension(3,ndf_w2,nqp_h,nqp_v), intent(in) :: w2_basis
    real(kind=r_def), dimension(3,ndf_w1,nqp_h,nqp_v), intent(in) :: w1_basis
    real(kind=r_def), dimension(1,ndf_chi,nqp_h,nqp_v), intent(in) :: chi_basis
    real(kind=r_def), dimension(3,ndf_chi,nqp_h,nqp_v), intent(in) :: chi_diff_basis
    real(kind=r_def), dimension(undf_w2), intent(in) :: r_u
    real(kind=r_def), dimension(undf_w2), intent(inout) :: wind
    real(kind=r_def), dimension(undf_w1), intent(inout) :: vorticity
    real(kind=r_def), dimension(undf_w2), intent(in) :: ls_wind
    real(kind=r_def), dimension(undf_w1), intent(in) :: ls_vorticity
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi_1
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi_2
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi_3
    real(kind=r_def), dimension(undf_pid), intent(in) :: panel_id
    real(kind=r_def), dimension(nqp_h), intent(in) :: wqp_h
    real(kind=r_def), dimension(nqp_v), intent(in) :: wqp_v
    integer(kind=i_def) :: df
    integer(kind=i_def) :: k
    integer(kind=i_def) :: loc
    integer(kind=i_def) :: ipanel
    integer(kind=r_def) :: qp1
    integer(kind=r_def) :: qp2
    real(kind=r_def), dimension(ndf_chi) :: chi_1_e
    real(kind=r_def), dimension(ndf_chi) :: chi_2_e
    real(kind=r_def), dimension(ndf_chi) :: chi_3_e
    real(kind=r_def) :: dj
    real(kind=r_def), dimension(3,3) :: jac
    real(kind=r_def), dimension(3,3) :: jac_inv
    real(kind=r_def), dimension(3) :: vorticity_at_quad
    real(kind=r_def), dimension(3) :: u_at_quad
    real(kind=r_def), dimension(3) :: ls_vorticity_at_quad
    real(kind=r_def), dimension(3) :: u_ls_at_quad
    real(kind=r_def), dimension(3) :: j_vorticity
    real(kind=r_def), dimension(3) :: j_ls_vorticity
    real(kind=r_def), dimension(3) :: vorticity_term
    real(kind=r_def), dimension(3,3) :: jac_inv_transpose
    real(kind=r_def), dimension(3) :: mul1
    real(kind=r_def), dimension(3) :: mul2
    real(kind=r_def), dimension(3) :: cross_product1
    real(kind=r_def), dimension(3) :: cross_product2
    integer :: i
    real(kind=r_def) :: res_dot_product
    integer :: idx
    integer :: idx_1
    integer :: idx_2
    integer :: idx_3
    integer :: j
    integer :: ii
    integer :: i_1
    integer :: j_1
    integer :: i_2
    integer :: j_2

    res_dot_product = 0.0_r_def
    vorticity_term = 0.0_r_def
    cross_product1 = 0.0_r_def
    cross_product2 = 0.0_r_def
    j_vorticity = 0.0_r_def
    u_at_quad = 0.0_r_def
    mul2 = 0.0_r_def
    vorticity_at_quad = 0.0_r_def
    ipanel = INT(panel_id(map_pid(1)), i_def)
    do k = nlayers - 1, 0, -1
      do df = 1, ndf_chi, 1
        loc = k + map_chi(df)
        chi_1_e(df) = chi_1(loc)
        chi_2_e(df) = chi_2(loc)
        chi_3_e(df) = chi_3(loc)
      enddo
      do qp2 = nqp_v, 1, -1
        do qp1 = nqp_h, 1, -1
          call pointwise_coordinate_jacobian(ndf_chi, chi_1_e(:), chi_2_e(:), chi_3_e(:), ipanel, chi_basis(:,:,qp1,qp2), &
&chi_diff_basis(:,:,qp1,qp2), jac(:,:), dj)
          jac_inv(:,:) = pointwise_coordinate_jacobian_inverse(jac(:,:),dj)
          jac_inv_transpose(:,:) = TRANSPOSE(jac_inv)
          do j = 1, 3, 1
            do i = 1, 3, 1
              jac(i,j) = 0.0
              do ii = 1, 3, 1
                jac(i,j) = jac(i,j) + jac_inv(i,ii) * jac_inv_transpose(ii,j)
              enddo
            enddo
          enddo
          ls_vorticity_at_quad(:) = 0.0_r_def
          do df = 1, ndf_w1, 1
            ls_vorticity_at_quad(:) = ls_vorticity_at_quad(:) + ls_vorticity(map_w1(df) + k) * w1_basis(:,df,qp1,qp2)
          enddo
          u_ls_at_quad(:) = 0.0_r_def
          do df = 1, ndf_w2, 1
            u_ls_at_quad(:) = u_ls_at_quad(:) + ls_wind(map_w2(df) + k) * w2_basis(:,df,qp1,qp2)
          enddo
          do i_1 = 1, 3, 1
            mul1(i_1) = 0.0
            do j_1 = 1, 3, 1
              mul1(i_1) = jac(i_1,j_1) * ls_vorticity_at_quad(j_1) + mul1(i_1)
            enddo
          enddo
          j_ls_vorticity(:) = wqp_h(qp1) * wqp_v(qp2) * mul1(:)
          do df = ndf_w2, 1, -1
            res_dot_product = res_dot_product + (-r_u(map_w2(df) + k))
            do i = 3, 1, -1
              vorticity_term(i) = vorticity_term(i) + res_dot_product * w2_basis(i,df,qp1,qp2)
            enddo
            res_dot_product = 0.0
          enddo
          do idx_3 = UBOUND(vorticity_term, 1), LBOUND(vorticity_term, 1), -1
            cross_product1(idx_3) = cross_product1(idx_3) + vorticity_term(idx_3)
            cross_product2(idx_3) = cross_product2(idx_3) + vorticity_term(idx_3)
            vorticity_term(idx_3) = 0.0
          enddo
          j_vorticity(1) = j_vorticity(1) + cross_product2(3) * u_ls_at_quad(2)
          j_vorticity(2) = j_vorticity(2) - cross_product2(3) * u_ls_at_quad(1)
          cross_product2(3) = 0.0
          j_vorticity(1) = j_vorticity(1) + (-cross_product2(2) * u_ls_at_quad(3))
          j_vorticity(3) = j_vorticity(3) + cross_product2(2) * u_ls_at_quad(1)
          cross_product2(2) = 0.0
          j_vorticity(2) = j_vorticity(2) + cross_product2(1) * u_ls_at_quad(3)
          j_vorticity(3) = j_vorticity(3) - cross_product2(1) * u_ls_at_quad(2)
          cross_product2(1) = 0.0
          u_at_quad(2) = u_at_quad(2) + j_ls_vorticity(1) * cross_product1(3)
          u_at_quad(1) = u_at_quad(1) - j_ls_vorticity(2) * cross_product1(3)
          cross_product1(3) = 0.0
          u_at_quad(3) = u_at_quad(3) + (-j_ls_vorticity(1) * cross_product1(2))
          u_at_quad(1) = u_at_quad(1) + j_ls_vorticity(3) * cross_product1(2)
          cross_product1(2) = 0.0
          u_at_quad(3) = u_at_quad(3) + j_ls_vorticity(2) * cross_product1(1)
          u_at_quad(2) = u_at_quad(2) - j_ls_vorticity(3) * cross_product1(1)
          cross_product1(1) = 0.0
          do idx_2 = UBOUND(j_vorticity, 1), LBOUND(j_vorticity, 1), -1
            mul2(idx_2) = mul2(idx_2) + j_vorticity(idx_2) * wqp_h(qp1) * wqp_v(qp2)
            j_vorticity(idx_2) = 0.0
          enddo
          do i_2 = 3, 1, -1
            do j_2 = 3, 1, -1
              vorticity_at_quad(j_2) = vorticity_at_quad(j_2) + jac(i_2,j_2) * mul2(i_2)
            enddo
            mul2(i_2) = 0.0
          enddo
          do df = ndf_w2, 1, -1
            do idx_2 = UBOUND(u_at_quad, 1), LBOUND(u_at_quad, 1), -1
              wind(k + map_w2(df)) = wind(k + map_w2(df)) + w2_basis(idx_2,df,qp1,qp2) * u_at_quad(idx_2)
            enddo
          enddo
          do idx_1 = UBOUND(u_at_quad, 1), LBOUND(u_at_quad, 1), -1
            u_at_quad(idx_1) = 0.0
          enddo
          do df = ndf_w1, 1, -1
            do idx_1 = UBOUND(vorticity_at_quad, 1), LBOUND(vorticity_at_quad, 1), -1
              vorticity(k + map_w1(df)) = vorticity(k + map_w1(df)) + vorticity_at_quad(idx_1) * w1_basis(idx_1,df,qp1,qp2)
            enddo
          enddo
          do idx = UBOUND(vorticity_at_quad, 1), LBOUND(vorticity_at_quad, 1), -1
            vorticity_at_quad(idx) = 0.0
          enddo
        enddo
      enddo
    enddo

  end subroutine adj_vorticity_advection_code

end module adj_vorticity_advection_kernel_mod
