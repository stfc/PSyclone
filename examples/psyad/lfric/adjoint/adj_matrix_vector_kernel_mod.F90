module adj_matrix_vector_kernel_mod
  use argument_mod, only : any_space_1, any_space_2, arg_type, cell_column, gh_field, gh_inc, gh_operator, gh_read, gh_real
  use constants_mod, only : i_def, r_double, r_single
  use kernel_mod, only : kernel_type
  implicit none
  interface adj_matrix_vector_code
  module procedure adj_matrix_vector_code_r_single, adj_matrix_vector_code_r_double
end interface
  type, public, extends(kernel_type) :: adj_matrix_vector_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(3) = (/arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_1), arg_type(GH_FIELD, GH_REAL, GH_INC, &
&ANY_SPACE_2), arg_type(GH_OPERATOR, GH_REAL, GH_READ, ANY_SPACE_1, ANY_SPACE_2)/)
  INTEGER :: operates_on = CELL_COLUMN
END TYPE
  private

  public :: adj_matrix_vector_code
  private :: adj_matrix_vector_code_r_single, adj_matrix_vector_code_r_double

  contains
  subroutine adj_matrix_vector_code_r_single(cell, nlayers, lhs, x, ncell_3d, matrix, ndf1, undf1, map1, ndf2, undf2, map2)
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
    real(kind=r_single), dimension(ncell_3d,ndf1,ndf2), intent(in) :: matrix
    integer(kind=i_def) :: df
    integer(kind=i_def) :: df2
    integer(kind=i_def) :: k
    integer(kind=i_def) :: ik

    do df = ndf1, 1, -1
      do df2 = ndf2, 1, -1
        do k = nlayers - 1, 0, -1
          ik = cell * nlayers + k - nlayers + 1
          x(k + map2(df2)) = x(k + map2(df2)) + matrix(ik,df,df2) * lhs(map1(df) + k)
        enddo
      enddo
    enddo

  end subroutine adj_matrix_vector_code_r_single
  subroutine adj_matrix_vector_code_r_double(cell, nlayers, lhs, x, ncell_3d, matrix, ndf1, undf1, map1, ndf2, undf2, map2)
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
    real(kind=r_double), dimension(ncell_3d,ndf1,ndf2), intent(in) :: matrix
    integer(kind=i_def) :: df
    integer(kind=i_def) :: df2
    integer(kind=i_def) :: k
    integer(kind=i_def) :: ik

    do df = ndf1, 1, -1
      do df2 = ndf2, 1, -1
        do k = nlayers - 1, 0, -1
          ik = cell * nlayers + k - nlayers + 1
          x(k + map2(df2)) = x(k + map2(df2)) + matrix(ik,df,df2) * lhs(map1(df) + k)
        enddo
      enddo
    enddo

  end subroutine adj_matrix_vector_code_r_double

end module adj_matrix_vector_kernel_mod
