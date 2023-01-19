module adj_dg_inc_matrix_vector_kernel_mod
  use argument_mod, only : any_discontinuous_space_1, any_space_1, arg_type, &
       cell_column, gh_field, gh_operator, gh_read, gh_inc, &
       gh_readwrite, gh_real
  use constants_mod, only : i_def, r_double, r_single
  use kernel_mod, only : kernel_type
  implicit none
  interface adj_dg_inc_matrix_vector_code
  module procedure adj_dg_inc_matrix_vector_code_r_single, adj_dg_inc_matrix_vector_code_r_double
end interface
  type, public, extends(kernel_type) :: adj_dg_inc_matrix_vector_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(3) = (/arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_DISCONTINUOUS_SPACE_1), arg_type(GH_FIELD, &
&GH_REAL, GH_INC, ANY_SPACE_1), arg_type(GH_OPERATOR, GH_REAL, GH_READ, ANY_DISCONTINUOUS_SPACE_1, ANY_SPACE_1)/)
  INTEGER :: operates_on = CELL_COLUMN
END TYPE
  private

  public :: adj_dg_inc_matrix_vector_code
  private :: adj_dg_inc_matrix_vector_code_r_single, adj_dg_inc_matrix_vector_code_r_double

  contains
  subroutine adj_dg_inc_matrix_vector_code_r_single(cell, nlayers, lhs, x, ncell_3d, matrix, ndf1, undf1, map1, ndf2, undf2, map2)
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: undf1
    integer(kind=i_def), intent(in) :: ndf1
    integer(kind=i_def), intent(in) :: undf2
    integer(kind=i_def), intent(in) :: ndf2
    integer(kind=i_def), dimension(ndf1), intent(in) :: map1
    integer(kind=i_def), dimension(ndf2), intent(in) :: map2
    real(kind=r_single), dimension(undf2), intent(inout) :: x
    real(kind=r_single), dimension(undf1), intent(in) :: lhs
    real(kind=r_single), dimension(ndf1,ndf2,ncell_3d), intent(in) :: matrix
    integer(kind=i_def) :: df
    integer(kind=i_def) :: k
    integer(kind=i_def) :: ik
    real(kind=r_single), dimension(ndf2) :: x_e
    real(kind=r_single), dimension(ndf1) :: lhs_e
    integer :: i
    integer :: j

    lhs_e = 0.0_r_single
    x_e = 0.0_r_single
    do k = nlayers - 1, 0, -1
      ik = cell * nlayers + k - nlayers + 1
      do df = ndf1, 1, -1
        lhs_e(df) = lhs_e(df) + lhs(map1(df) + k)
      enddo
      do i = ndf1, 1, -1
        do j = ndf2, 1, -1
          x_e(j) = x_e(j) + matrix(i,j,ik) * lhs_e(i)
        enddo
        lhs_e(i) = 0.0
      enddo
      do df = ndf2, 1, -1
        x(map2(df) + k) = x(map2(df) + k) + x_e(df)
        x_e(df) = 0.0
      enddo
    enddo

  end subroutine adj_dg_inc_matrix_vector_code_r_single
  subroutine adj_dg_inc_matrix_vector_code_r_double(cell, nlayers, lhs, x, ncell_3d, matrix, ndf1, undf1, map1, ndf2, undf2, map2)
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: undf1
    integer(kind=i_def), intent(in) :: ndf1
    integer(kind=i_def), intent(in) :: undf2
    integer(kind=i_def), intent(in) :: ndf2
    integer(kind=i_def), dimension(ndf1), intent(in) :: map1
    integer(kind=i_def), dimension(ndf2), intent(in) :: map2
    real(kind=r_double), dimension(undf2), intent(inout) :: x
    real(kind=r_double), dimension(undf1), intent(in) :: lhs
    real(kind=r_double), dimension(ndf1,ndf2,ncell_3d), intent(in) :: matrix
    integer(kind=i_def) :: df
    integer(kind=i_def) :: k
    integer(kind=i_def) :: ik
    real(kind=r_double), dimension(ndf2) :: x_e
    real(kind=r_double), dimension(ndf1) :: lhs_e
    integer :: i
    integer :: j

    lhs_e = 0.0_r_double
    x_e = 0.0_r_double
    do k = nlayers - 1, 0, -1
      ik = cell * nlayers + k - nlayers + 1
      do df = ndf1, 1, -1
        lhs_e(df) = lhs_e(df) + lhs(map1(df) + k)
      enddo
      do i = ndf1, 1, -1
        do j = ndf2, 1, -1
          x_e(j) = x_e(j) + matrix(i,j,ik) * lhs_e(i)
        enddo
        lhs_e(i) = 0.0
      enddo
      do df = ndf2, 1, -1
        x(map2(df) + k) = x(map2(df) + k) + x_e(df)
        x_e(df) = 0.0
      enddo
    enddo

  end subroutine adj_dg_inc_matrix_vector_code_r_double

end module adj_dg_inc_matrix_vector_kernel_mod
