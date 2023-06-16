module adj_strong_curl_kernel_mod
  use argument_mod, only : arg_type, cell_column, func_type, gh_basis, &
       gh_diff_basis, gh_evaluator, gh_field, gh_inc, gh_real
  use constants_mod, only : i_def, r_def
  use fs_continuity_mod, only : w1, w2
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_strong_curl_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(2) = (/ &
       arg_type(GH_FIELD, GH_REAL, GH_INC, W2), &
       arg_type(GH_FIELD, GH_REAL, GH_INC, W1)/)
  TYPE(func_type) :: meta_funcs(2) = (/ &
       func_type(W2, GH_BASIS), &
       func_type(W1, GH_DIFF_BASIS)/)
  INTEGER :: operates_on = CELL_COLUMN
  INTEGER :: gh_shape = GH_EVALUATOR
  CONTAINS
  PROCEDURE, NOPASS :: adj_strong_curl_code
END TYPE
  private

  public :: adj_strong_curl_code

  contains
    subroutine adj_strong_curl_code( &
         nlayers, xi, u, &
         ndf2, undf2, map2, basis_w2,  basis_w2_on_w1, &
         ndf1, undf1, map1, diff_basis_w1, diff_basis_w1_on_w1)
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf1
    integer(kind=i_def), intent(in) :: undf1
    integer(kind=i_def), intent(in) :: ndf2
    integer(kind=i_def), intent(in) :: undf2
    integer(kind=i_def), dimension(ndf1), intent(in) :: map1
    integer(kind=i_def), dimension(ndf2), intent(in) :: map2
    real(kind=r_def), dimension(3,ndf2,ndf2), intent(in) :: basis_w2
    REAL(KIND=r_def), intent(in), dimension(3,ndf2,ndf1) :: basis_w2_on_w1
    real(kind=r_def), dimension(3,ndf1,ndf2), intent(in) :: diff_basis_w1
    REAL(KIND=r_def), intent(in), dimension(3,ndf1,ndf1) :: diff_basis_w1_on_w1
    real(kind=r_def), dimension(undf2), intent(inout) :: xi
    real(kind=r_def), dimension(undf1), intent(inout) :: u
    integer(kind=i_def) :: df1
    integer(kind=i_def) :: df2
    integer(kind=i_def) :: k
    real(kind=r_def), dimension(3) :: curl_u
    integer :: i
    real(kind=r_def) :: res_dot_product
    integer :: idx
    integer :: idx_1

    res_dot_product = 0.0_r_def
    curl_u = 0.0_r_def
    do k = nlayers - 1, 0, -1
      do df2 = ndf2, 1, -1
        res_dot_product = res_dot_product + xi(map2(df2) + k)
        xi(map2(df2) + k) = 0.0
        do i = 3, 1, -1
          curl_u(i) = curl_u(i) + basis_w2(i,df2,df2) * res_dot_product
        enddo
        res_dot_product = 0.0
        do df1 = ndf1, 1, -1
          do idx_1 = UBOUND(curl_u, 1), LBOUND(curl_u, 1), -1
            u(k + map1(df1)) = u(k + map1(df1)) + diff_basis_w1(idx_1,df1,df2) * curl_u(idx_1)
          enddo
        enddo
        do idx = UBOUND(curl_u, 1), LBOUND(curl_u, 1), -1
          curl_u(idx) = 0.0
        enddo
      enddo
    enddo

  end subroutine adj_strong_curl_code

end module adj_strong_curl_kernel_mod
