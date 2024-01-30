module adj_convert_hdiv_field_kernel_mod
  use kernel_mod, only : kernel_type
  use argument_mod, only : any_discontinuous_space_3, any_space_1, any_space_2, wchi, arg_type, cell_column, func_type, &
&gh_basis, gh_diff_basis, gh_evaluator, gh_field, gh_inc, gh_read, gh_real
  use constants_mod, only : i_def, r_def
  implicit none
  type, public, extends(kernel_type) :: adj_convert_hdiv_field_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(4) = (/arg_type(GH_FIELD * 3, GH_REAL, GH_READ, ANY_SPACE_1), arg_type(GH_FIELD, GH_REAL, GH_INC, &
&ANY_SPACE_2), arg_type(GH_FIELD * 3, GH_REAL, GH_READ, WCHI), arg_type(GH_FIELD, GH_REAL, GH_READ, &
&ANY_DISCONTINUOUS_SPACE_3)/)
  TYPE(func_type) :: meta_funcs(2) = (/func_type(ANY_SPACE_2, GH_BASIS), func_type(WCHI, GH_BASIS, GH_DIFF_BASIS)/)
  INTEGER :: operates_on = CELL_COLUMN
  INTEGER :: gh_shape = GH_EVALUATOR
  CONTAINS
  PROCEDURE, NOPASS :: adj_convert_hdiv_field_code
END TYPE
  private

  public :: adj_convert_hdiv_field_code

  contains
  subroutine adj_convert_hdiv_field_code(nlayers, physical_field1, physical_field2, physical_field3, computational_field, chi1, &
&chi2, chi3, panel_id, ndf1, undf1, map1, ndf2, undf2, map2, basis2, ndf_chi, undf_chi, map_chi, basis_chi, diff_basis_chi, &
&ndf_pid, undf_pid, map_pid)
    use coordinate_jacobian_mod, only : coordinate_jacobian
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf1
    integer(kind=i_def), intent(in) :: undf1
    integer(kind=i_def), intent(in) :: ndf2
    integer(kind=i_def), intent(in) :: undf2
    integer(kind=i_def), intent(in) :: ndf_chi
    integer(kind=i_def), intent(in) :: undf_chi
    integer(kind=i_def), intent(in) :: ndf_pid
    integer(kind=i_def), intent(in) :: undf_pid
    integer(kind=i_def), dimension(ndf1), intent(in) :: map1
    integer(kind=i_def), dimension(ndf2), intent(in) :: map2
    integer(kind=i_def), dimension(ndf_chi), intent(in) :: map_chi
    integer(kind=i_def), dimension(ndf_pid), intent(in) :: map_pid
    real(kind=r_def), dimension(undf2), intent(inout) :: computational_field
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi1
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi2
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi3
    real(kind=r_def), dimension(undf_pid), intent(in) :: panel_id
    real(kind=r_def), dimension(undf1), intent(in) :: physical_field1
    real(kind=r_def), dimension(undf1), intent(in) :: physical_field2
    real(kind=r_def), dimension(undf1), intent(in) :: physical_field3
    real(kind=r_def), dimension(1,ndf_chi,ndf1), intent(in) :: basis_chi
    real(kind=r_def), dimension(3,ndf_chi,ndf1), intent(in) :: diff_basis_chi
    real(kind=r_def), dimension(3,ndf2,ndf1), intent(in) :: basis2
    integer(kind=i_def) :: df
    integer(kind=i_def) :: df2
    integer(kind=i_def) :: k
    real(kind=r_def), dimension(3,3,ndf1) :: jacobian
    real(kind=r_def), dimension(ndf1) :: dj
    real(kind=r_def), dimension(3) :: vector_in
    real(kind=r_def), dimension(3) :: vector_out
    real(kind=r_def), dimension(ndf_chi) :: chi1_e
    real(kind=r_def), dimension(ndf_chi) :: chi2_e
    real(kind=r_def), dimension(ndf_chi) :: chi3_e
    integer(kind=i_def) :: ipanel
    integer :: idx
    integer :: idx_1
    integer :: i
    integer :: j

    vector_out = 0.0_r_def
    vector_in = 0.0_r_def
    ipanel = INT(panel_id(map_pid(1)), i_def)
    do k = nlayers - 1, 0, -1
      do df = 1, ndf_chi, 1
        chi1_e(df) = chi1(map_chi(df) + k)
        chi2_e(df) = chi2(map_chi(df) + k)
        chi3_e(df) = chi3(map_chi(df) + k)
      enddo
      call coordinate_jacobian(ndf_chi, ndf1, chi1_e(:), chi2_e(:), chi3_e(:), ipanel, basis_chi(:,:,:), diff_basis_chi(:,:,:), &
&jacobian(:,:,:), dj(:))
      do df = ndf1, 1, -1
        vector_out(3) = vector_out(3) + physical_field3(map1(df) + k)
        vector_out(2) = vector_out(2) + physical_field2(map1(df) + k)
        vector_out(1) = vector_out(1) + physical_field1(map1(df) + k)
        do idx_1 = UBOUND(vector_out, 1), LBOUND(vector_out, 1), -1
          vector_out(idx_1) = vector_out(idx_1) / dj(df)
        enddo
        do i = 3, 1, -1
          do j = 3, 1, -1
            vector_in(j) = vector_in(j) + jacobian(i,j,df) * vector_out(i)
          enddo
          vector_out(i) = 0.0
        enddo
        do df2 = ndf2, 1, -1
          do idx_1 = UBOUND(vector_in, 1), LBOUND(vector_in, 1), -1
            computational_field(k + map2(df2)) = computational_field(k + map2(df2)) + basis2(idx_1,df2,df) * vector_in(idx_1)
          enddo
        enddo
        do idx = UBOUND(vector_in, 1), LBOUND(vector_in, 1), -1
          vector_in(idx) = 0.0
        enddo
      enddo
    enddo

  end subroutine adj_convert_hdiv_field_code

end module adj_convert_hdiv_field_kernel_mod
