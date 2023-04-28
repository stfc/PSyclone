module adj_combine_w2_field_kernel_mod
  use argument_mod, only : arg_type, cell_column, gh_field, gh_inc, gh_readwrite, gh_real
  use constants_mod, only : i_def, r_def
  use fs_continuity_mod, only : w2, w2h, w2v
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_combine_w2_field_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(3) = (/arg_type(GH_FIELD, GH_REAL, GH_INC, W2), arg_type(GH_FIELD, GH_REAL, GH_INC, W2h), &
&arg_type(GH_FIELD, GH_REAL, GH_READWRITE, W2v)/)
  INTEGER :: operates_on = CELL_COLUMN
  CONTAINS
  PROCEDURE, NOPASS :: adj_combine_w2_field_code
END TYPE
  private

  public :: adj_combine_w2_field_code

  contains
  subroutine adj_combine_w2_field_code(nlayers, uvw, uv, w, ndf_w2, undf_w2, map_w2, ndf_w2h, undf_w2h, map_w2h, ndf_w2v, &
&undf_w2v, map_w2v)
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_w2h
    integer(kind=i_def), intent(in) :: ndf_w2v
    integer(kind=i_def), intent(in) :: undf_w2
    integer(kind=i_def), intent(in) :: undf_w2h
    integer(kind=i_def), intent(in) :: undf_w2v
    integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
    integer(kind=i_def), dimension(ndf_w2h), intent(in) :: map_w2h
    integer(kind=i_def), dimension(ndf_w2v), intent(in) :: map_w2v
    real(kind=r_def), dimension(undf_w2), intent(inout) :: uvw
    real(kind=r_def), dimension(undf_w2h), intent(inout) :: uv
    real(kind=r_def), dimension(undf_w2v), intent(inout) :: w
    integer(kind=i_def) :: df
    integer(kind=i_def) :: k

    do k = nlayers - 1, 0, -1
      do df = 2, 1, -1
        w(map_w2v(df) + k) = w(map_w2v(df) + k) + uvw(map_w2(4 + df) + k)
        uvw(map_w2(4 + df) + k) = 0.0
      enddo
      do df = 4, 1, -1
        uv(map_w2h(df) + k) = uv(map_w2h(df) + k) + uvw(map_w2(df) + k)
        uvw(map_w2(df) + k) = 0.0
      enddo
    enddo

  end subroutine adj_combine_w2_field_code

end module adj_combine_w2_field_kernel_mod
