module adj_dg_matrix_vector_kernel_mod
  use constants_mod, only : i_def, r_def
  use kernel_mod, only : kernel_type
  use argument_mod, only : any_discontinuous_space_1, any_space_1, arg_type, cell_column, gh_field, gh_operator, gh_read, gh_real, &
&gh_readwrite, gh_inc
  implicit none
  type, public, extends(kernel_type) :: adj_dg_matrix_vector_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(3) = (/arg_type(GH_FIELD, GH_REAL, GH_READWRITE, ANY_DISCONTINUOUS_SPACE_1), arg_type(GH_FIELD, GH_REAL, &
&GH_INC, ANY_SPACE_1), arg_type(GH_OPERATOR, GH_REAL, GH_READ, ANY_DISCONTINUOUS_SPACE_1, ANY_SPACE_1)/)
  INTEGER :: operates_on = CELL_COLUMN
  CONTAINS
  PROCEDURE, NOPASS :: adj_dg_matrix_vector_code
END TYPE
  private

  public :: adj_dg_matrix_vector_code

  contains
  subroutine adj_dg_matrix_vector_code(cell, nlayers, lhs, x, ncell_3d, matrix, ndf1, undf1, map1, ndf2, undf2, map2)
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: undf1
    integer(kind=i_def), intent(in) :: ndf1
    integer(kind=i_def), intent(in) :: undf2
    integer(kind=i_def), intent(in) :: ndf2
    integer(kind=i_def), dimension(ndf1), intent(in) :: map1
    integer(kind=i_def), dimension(ndf2), intent(in) :: map2
    real(kind=r_def), dimension(undf2), intent(inout) :: x
    real(kind=r_def), dimension(undf1), intent(inout) :: lhs
    real(kind=r_def), dimension(ndf1,ndf2,ncell_3d), intent(in) :: matrix
    integer(kind=i_def) :: df
    integer(kind=i_def) :: k
    integer(kind=i_def) :: ik
    real(kind=r_def), dimension(ndf2) :: x_e
    real(kind=r_def), dimension(ndf1) :: lhs_e
    integer :: i
    integer :: j

    lhs_e = 0.0_r_def
    x_e = 0.0_r_def
    do k = nlayers - 1, 0, -1
      ik = cell * nlayers + k - nlayers + 1
      do df = ndf1, 1, -1
        lhs_e(df) = lhs_e(df) + lhs(map1(df) + k)
        lhs(map1(df) + k) = 0.0
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

  end subroutine adj_dg_matrix_vector_code

end module adj_dg_matrix_vector_kernel_mod
