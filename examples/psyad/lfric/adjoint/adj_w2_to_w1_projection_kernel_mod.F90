module adj_w2_to_w1_projection_kernel_mod
  use argument_mod, only : arg_type, cell_column, func_type, gh_basis, gh_field, gh_inc, gh_quadrature_xyoz, gh_read, gh_real
  use constants_mod, only : i_def, r_def
  use fs_continuity_mod, only : w1, w2
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_w2_to_w1_projection_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(2) = (/arg_type(GH_FIELD, GH_REAL, GH_READ, W1), arg_type(GH_FIELD, GH_REAL, GH_INC, W2)/)
  TYPE(func_type) :: meta_funcs(2) = (/func_type(W1, GH_BASIS), func_type(W2, GH_BASIS)/)
  INTEGER :: operates_on = CELL_COLUMN
  INTEGER :: gh_shape = GH_QUADRATURE_XYoZ
  CONTAINS
  PROCEDURE, NOPASS :: adj_w2_to_w1_projection_code
END TYPE
  private

  public :: adj_w2_to_w1_projection_code

  contains
  subroutine adj_w2_to_w1_projection_code(nlayers, v_w1, u_w2, ndf1, undf1, map1, basis_w1, ndf2, undf2, map2, basis_w2, nqp_h, &
&nqp_v, wqp_h, wqp_v)
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf1
    integer(kind=i_def), intent(in) :: undf1
    integer(kind=i_def), intent(in) :: ndf2
    integer(kind=i_def), intent(in) :: undf2
    integer(kind=i_def), intent(in) :: nqp_h
    integer(kind=i_def), intent(in) :: nqp_v
    integer(kind=i_def), dimension(ndf1), intent(in) :: map1
    integer(kind=i_def), dimension(ndf2), intent(in) :: map2
    real(kind=r_def), dimension(3,ndf1,nqp_h,nqp_v), intent(in) :: basis_w1
    real(kind=r_def), dimension(3,ndf2,nqp_h,nqp_v), intent(in) :: basis_w2
    real(kind=r_def), dimension(undf1), intent(in) :: v_w1
    real(kind=r_def), dimension(undf2), intent(inout) :: u_w2
    real(kind=r_def), dimension(nqp_h), intent(in) :: wqp_h
    real(kind=r_def), dimension(nqp_v), intent(in) :: wqp_v
    integer(kind=i_def) :: df1
    integer(kind=i_def) :: df2
    integer(kind=i_def) :: k
    integer(kind=i_def) :: qp1
    integer(kind=i_def) :: qp2
    real(kind=r_def), dimension(3) :: wind
    real(kind=r_def) :: vu
    integer :: i
    real(kind=r_def) :: res_dot_product
    integer :: idx
    integer :: idx_1

    vu = 0.0_r_def
    res_dot_product = 0.0_r_def
    wind = 0.0_r_def
    do k = nlayers - 1, 0, -1
      do qp2 = nqp_v, 1, -1
        do qp1 = nqp_h, 1, -1
          do df1 = ndf1, 1, -1
            vu = vu + v_w1(map1(df1) + k)
            res_dot_product = res_dot_product + vu * wqp_h(qp1) * wqp_v(qp2)
            vu = 0.0
            do i = 3, 1, -1
              wind(i) = wind(i) + basis_w1(i,df1,qp1,qp2) * res_dot_product
            enddo
            res_dot_product = 0.0
          enddo
          do df2 = ndf2, 1, -1
            do idx_1 = 3, 1, -1
              u_w2(k + map2(df2)) = u_w2(k + map2(df2)) + basis_w2(idx_1,df2,qp1,qp2) * wind(idx_1)
            enddo
          enddo
          do idx = 3, 1, -1
            wind(idx) = 0.0
          enddo
        enddo
      enddo
    enddo

  end subroutine adj_w2_to_w1_projection_code

end module adj_w2_to_w1_projection_kernel_mod
