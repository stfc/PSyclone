module adj_apply_variable_hx_kernel_mod
  use argument_mod, only : arg_type, cell_column, gh_field, gh_operator, gh_read, gh_real, gh_scalar, gh_write
  use constants_mod, only : i_def, r_double, r_single
  use fs_continuity_mod, only : w2, w3, wtheta
  use kernel_mod, only : kernel_type
  implicit none
  interface adj_apply_variable_hx_code
  module procedure adj_apply_variable_hx_code_r_single, adj_apply_variable_hx_code_r_double
end interface
  type, public, extends(kernel_type) :: adj_apply_variable_hx_kernel_type
  type(ARG_TYPE) :: META_ARGS(9) = (/ &
    arg_type(gh_field, gh_real, gh_write, w3), &
    arg_type(gh_field, gh_real, gh_read, w2), &
    arg_type(gh_field, gh_real, gh_read, wtheta), &
    arg_type(gh_field, gh_real, gh_read, w3), &
    arg_type(gh_operator, gh_real, gh_read, w3, w2), &
    arg_type(gh_operator, gh_real, gh_read, w3, wtheta), &
    arg_type(gh_operator, gh_real, gh_read, wtheta, w2), &
    arg_type(gh_operator, gh_real, gh_read, w3, w3), &
    arg_type(gh_scalar, gh_real, gh_read)/)
  INTEGER :: OPERATES_ON = cell_column
END TYPE adj_apply_variable_hx_kernel_type

  private

  public :: adj_apply_variable_hx_code

  contains
  subroutine adj_apply_variable_hx_code_r_double(cell, nlayers, lhs, x, mt_inv, pressure, ncell_3d_1, div, ncell_3d_2, p3t, &
&ncell_3d_3, pt2, ncell_3d_4, m3, sgn, ndf_w3, undf_w3, map_w3, ndf_w2, undf_w2, map_w2, ndf_wt, undf_wt, map_wt)
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_3d_1
    integer(kind=i_def), intent(in) :: ncell_3d_2
    integer(kind=i_def), intent(in) :: ncell_3d_3
    integer(kind=i_def), intent(in) :: ncell_3d_4
    integer(kind=i_def), intent(in) :: undf_w2
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_wt
    integer(kind=i_def), intent(in) :: ndf_wt
    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
    integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
    integer(kind=i_def), dimension(ndf_wt), intent(in) :: map_wt
    real(kind=r_double), dimension(undf_w2), intent(inout) :: x
    real(kind=r_double), dimension(undf_wt), intent(in) :: mt_inv
    real(kind=r_double), dimension(undf_w3), intent(inout) :: lhs
    real(kind=r_double), dimension(undf_w3), intent(inout) :: pressure
    real(kind=r_double), intent(in) :: sgn
    real(kind=r_double), dimension(ndf_w3,ndf_w2,ncell_3d_1), intent(in) :: div
    real(kind=r_double), dimension(ndf_wt,ndf_w2,ncell_3d_2), intent(in) :: pt2
    real(kind=r_double), dimension(ndf_w3,ndf_wt,ncell_3d_3), intent(in) :: p3t
    real(kind=r_double), dimension(ndf_w3,ndf_w3,ncell_3d_4), intent(in) :: m3
    integer(kind=i_def) :: df
    integer(kind=i_def) :: k
    integer(kind=i_def) :: ik
    integer(kind=i_def) :: is
    integer(kind=i_def) :: ie
    real(kind=r_double), dimension(ndf_w2) :: x_e
    real(kind=r_double), dimension(ndf_w3) :: lhs_e
    real(kind=r_double), dimension(ndf_w3) :: p_e
    real(kind=r_double), dimension(ndf_w3) :: lhs_e_div
    real(kind=r_double), dimension(ndf_w3) :: lhs_e_m3
    real(kind=r_double), dimension(ndf_w3) :: lhs_e_p3t
    real(kind=r_double), dimension(ndf_wt) :: t_e
    real(kind=r_double), allocatable, dimension(:) :: t
    integer :: idx
    integer :: idx_1
    integer :: i
    integer :: j
    integer :: i_1
    integer :: j_1
    integer :: i_2
    integer :: j_2
    integer :: i_3
    integer :: j_3

    lhs_e = 0.0_r_double
    lhs_e_m3 = 0.0_r_double
    lhs_e_p3t = 0.0_r_double
    lhs_e_div = 0.0_r_double
    p_e = 0.0_r_double
    t_e = 0.0_r_double
    x_e = 0.0_r_double
    is = MINVAL(map_wt(:))
    ie = nlayers + MAXVAL(map_wt(:)) - 1
    ALLOCATE(t(is:ie))
    t = 0.0_r_double
    do k = nlayers - 1, 0, -1
      ik = cell * nlayers + k - nlayers + 1
      do df = ndf_w3, 1, -1
        lhs_e(df) = lhs_e(df) + lhs(map_w3(df) + k)
        lhs(map_w3(df) + k) = 0.0
      enddo
      do idx_1 = ndf_w3, 1, -1
        lhs_e_div(idx_1) = lhs_e_div(idx_1) + sgn * lhs_e(idx_1)
        lhs_e_p3t(idx_1) = lhs_e_p3t(idx_1) + sgn * lhs_e(idx_1)
        lhs_e_m3(idx_1) = lhs_e_m3(idx_1) + lhs_e(idx_1)
        lhs_e(idx_1) = 0.0
      enddo
      do i_2 = ndf_w3, 1, -1
        do j_2 = ndf_w3, 1, -1
          p_e(j_2) = p_e(j_2) + m3(i_2,j_2,ik) * lhs_e_m3(i_2)
        enddo
        lhs_e_m3(i_2) = 0.0
      enddo
      do i_1 = ndf_w3, 1, -1
        do j_1 = ndf_wt, 1, -1
          t_e(j_1) = t_e(j_1) + p3t(i_1,j_1,ik) * lhs_e_p3t(i_1)
        enddo
        lhs_e_p3t(i_1) = 0.0
      enddo
      do i = ndf_w3, 1, -1
        do j = ndf_w2, 1, -1
          x_e(j) = x_e(j) + div(i,j,ik) * lhs_e_div(i)
        enddo
        lhs_e_div(i) = 0.0
      enddo
      do df = ndf_w3, 1, -1
        pressure(map_w3(df) + k) = pressure(map_w3(df) + k) + p_e(df)
        p_e(df) = 0.0
      enddo
      do df = ndf_w2, 1, -1
        x(map_w2(df) + k) = x(map_w2(df) + k) + x_e(df)
        x_e(df) = 0.0
      enddo
      do df = ndf_wt, 1, -1
        t(k + map_wt(df)) = t(k + map_wt(df)) + mt_inv(k + map_wt(df)) * t_e(df)
        t_e(df) = 0.0
      enddo
    enddo
    do k = nlayers - 1, 0, -1
      ik = cell * nlayers + k - nlayers + 1
      do df = ndf_wt, 1, -1
        t_e(df) = t_e(df) + t(map_wt(df) + k)
      enddo
      do i = ndf_wt, 1, -1
        do j = ndf_w2, 1, -1
          x_e(j) = x_e(j) + pt2(i,j,ik) * t_e(i)
        enddo
        t_e(i) = 0.0
      enddo
      do df = ndf_w2, 1, -1
        x(map_w2(df) + k) = x(map_w2(df) + k) + x_e(df)
        x_e(df) = 0.0
      enddo
    enddo
    do idx = ie, is, -1
      t(idx) = 0.0
    enddo
    DEALLOCATE(t)

  end subroutine adj_apply_variable_hx_code_r_double
  subroutine adj_apply_variable_hx_code_r_single(cell, nlayers, lhs, x, mt_inv, pressure, ncell_3d_1, div, ncell_3d_2, p3t, &
&ncell_3d_3, pt2, ncell_3d_4, m3, sgn, ndf_w3, undf_w3, map_w3, ndf_w2, undf_w2, map_w2, ndf_wt, undf_wt, map_wt)
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_3d_1
    integer(kind=i_def), intent(in) :: ncell_3d_2
    integer(kind=i_def), intent(in) :: ncell_3d_3
    integer(kind=i_def), intent(in) :: ncell_3d_4
    integer(kind=i_def), intent(in) :: undf_w2
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_wt
    integer(kind=i_def), intent(in) :: ndf_wt
    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
    integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
    integer(kind=i_def), dimension(ndf_wt), intent(in) :: map_wt
    real(kind=r_single), dimension(undf_w2), intent(inout) :: x
    real(kind=r_single), dimension(undf_wt), intent(in) :: mt_inv
    real(kind=r_single), dimension(undf_w3), intent(inout) :: lhs
    real(kind=r_single), dimension(undf_w3), intent(inout) :: pressure
    real(kind=r_single), intent(in) :: sgn
    real(kind=r_single), dimension(ndf_w3,ndf_w2,ncell_3d_1), intent(in) :: div
    real(kind=r_single), dimension(ndf_wt,ndf_w2,ncell_3d_2), intent(in) :: pt2
    real(kind=r_single), dimension(ndf_w3,ndf_wt,ncell_3d_3), intent(in) :: p3t
    real(kind=r_single), dimension(ndf_w3,ndf_w3,ncell_3d_4), intent(in) :: m3
    integer(kind=i_def) :: df
    integer(kind=i_def) :: k
    integer(kind=i_def) :: ik
    integer(kind=i_def) :: is
    integer(kind=i_def) :: ie
    real(kind=r_single), dimension(ndf_w2) :: x_e
    real(kind=r_single), dimension(ndf_w3) :: lhs_e
    real(kind=r_single), dimension(ndf_w3) :: p_e
    real(kind=r_single), dimension(ndf_w3) :: lhs_e_div
    real(kind=r_single), dimension(ndf_w3) :: lhs_e_m3
    real(kind=r_single), dimension(ndf_w3) :: lhs_e_p3t
    real(kind=r_single), dimension(ndf_wt) :: t_e
    real(kind=r_single), allocatable, dimension(:) :: t
    integer :: idx
    integer :: idx_1
    integer :: i
    integer :: j
    integer :: i_1
    integer :: j_1
    integer :: i_2
    integer :: j_2
    integer :: i_3
    integer :: j_3

    lhs_e = 0.0_r_single
    lhs_e_m3 = 0.0_r_single
    lhs_e_p3t = 0.0_r_single
    lhs_e_div = 0.0_r_single
    p_e = 0.0_r_single
    t_e = 0.0_r_single
    x_e = 0.0_r_single
    is = MINVAL(map_wt(:))
    ie = nlayers + MAXVAL(map_wt(:)) - 1
    ALLOCATE(t(is:ie))
    t = 0.0_r_single
    do k = nlayers - 1, 0, -1
      ik = cell * nlayers + k - nlayers + 1
      do df = ndf_w3, 1, -1
        lhs_e(df) = lhs_e(df) + lhs(map_w3(df) + k)
        lhs(map_w3(df) + k) = 0.0
      enddo
      do idx_1 = ndf_w3, 1, -1
        lhs_e_div(idx_1) = lhs_e_div(idx_1) + sgn * lhs_e(idx_1)
        lhs_e_p3t(idx_1) = lhs_e_p3t(idx_1) + sgn * lhs_e(idx_1)
        lhs_e_m3(idx_1) = lhs_e_m3(idx_1) + lhs_e(idx_1)
        lhs_e(idx_1) = 0.0
      enddo
      do i_2 = ndf_w3, 1, -1
        do j_2 = ndf_w3, 1, -1
          p_e(j_2) = p_e(j_2) + m3(i_2,j_2,ik) * lhs_e_m3(i_2)
        enddo
        lhs_e_m3(i_2) = 0.0
      enddo
      do i_1 = ndf_w3, 1, -1
        do j_1 = ndf_wt, 1, -1
          t_e(j_1) = t_e(j_1) + p3t(i_1,j_1,ik) * lhs_e_p3t(i_1)
        enddo
        lhs_e_p3t(i_1) = 0.0
      enddo
      do i = ndf_w3, 1, -1
        do j = ndf_w2, 1, -1
          x_e(j) = x_e(j) + div(i,j,ik) * lhs_e_div(i)
        enddo
        lhs_e_div(i) = 0.0
      enddo
      do df = ndf_w3, 1, -1
        pressure(map_w3(df) + k) = pressure(map_w3(df) + k) + p_e(df)
        p_e(df) = 0.0
      enddo
      do df = ndf_w2, 1, -1
        x(map_w2(df) + k) = x(map_w2(df) + k) + x_e(df)
        x_e(df) = 0.0
      enddo
      do df = ndf_wt, 1, -1
        t(k + map_wt(df)) = t(k + map_wt(df)) + mt_inv(k + map_wt(df)) * t_e(df)
        t_e(df) = 0.0
      enddo
    enddo
    do k = nlayers - 1, 0, -1
      ik = cell * nlayers + k - nlayers + 1
      do df = ndf_wt, 1, -1
        t_e(df) = t_e(df) + t(map_wt(df) + k)
      enddo
      do i = ndf_wt, 1, -1
        do j = ndf_w2, 1, -1
          x_e(j) = x_e(j) + pt2(i,j,ik) * t_e(i)
        enddo
        t_e(i) = 0.0
      enddo
      do df = ndf_w2, 1, -1
        x(map_w2(df) + k) = x(map_w2(df) + k) + x_e(df)
        x_e(df) = 0.0
      enddo
    enddo
    do idx = ie, is, -1
      t(idx) = 0.0
    enddo
    DEALLOCATE(t)

  end subroutine adj_apply_variable_hx_code_r_single

end module adj_apply_variable_hx_kernel_mod
