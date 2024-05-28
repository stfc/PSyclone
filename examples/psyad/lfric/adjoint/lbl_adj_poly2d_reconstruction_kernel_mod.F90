module adj_poly2d_reconstruction_lbl_kernel_mod
  use argument_mod, only : any_discontinuous_space_1, any_discontinuous_space_2,   &
       any_discontinuous_space_3, arg_type, cell_column,                           &
       func_type, gh_field, gh_integer, gh_read, gh_readwrite, gh_real, gh_scalar, &
       gh_write, region, stencil
  use constants_mod, only : i_def, l_def, r_tran
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_poly2d_reconstruction_lbl_kernel_type
    type(ARG_TYPE) :: META_ARGS(5) = (/                                                 &
      ! This is really gh_read but we need at least one arg. written to. This works because
      ! PSyclone does not check for consistency between the kernel metadata and its implementation.
      arg_type(gh_field, gh_real, gh_write, any_discontinuous_space_1),                 &
      ! This is really gh_readwrite but as it has a stencil we fake it.   
      arg_type(gh_field, gh_real, gh_read, any_discontinuous_space_2, stencil(region)), &
      arg_type(gh_field, gh_real, gh_read, any_discontinuous_space_3),                  &
      arg_type(gh_scalar, gh_integer, gh_read),                                         &
      arg_type(gh_scalar, gh_integer, gh_read)/)
      integer :: operates_on = cell_column
    contains
      procedure, nopass :: adj_poly2d_reconstruction_lbl_code
  end type adj_poly2d_reconstruction_lbl_kernel_type

  private

  public :: adj_poly2d_reconstruction_lbl_code

contains

  subroutine adj_poly2d_reconstruction_lbl_code(nlayers, reconstruction, tracer,   &
         cells_in_stencil, stencil_map, coeff, ndata, stencil_size, ndf_md, undf_md, &
         map_md, ndf_ws, undf_ws, map_ws, ndf_c, undf_c, map_c)
    integer(kind=i_def), parameter :: nfaces = 4
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_ws
    integer(kind=i_def), intent(in) :: undf_ws
    integer(kind=i_def), dimension(ndf_ws), intent(in) :: map_ws
    integer(kind=i_def), intent(in) :: ndf_md
    integer(kind=i_def), intent(in) :: undf_md
    integer(kind=i_def), dimension(ndf_md), intent(in) :: map_md
    integer(kind=i_def), intent(in) :: ndf_c
    integer(kind=i_def), intent(in) :: undf_c
    integer(kind=i_def), dimension(ndf_c), intent(in) :: map_c
    integer(kind=i_def), intent(in) :: ndata
    integer(kind=i_def), intent(in) :: cells_in_stencil
    integer(kind=i_def), intent(in) :: stencil_size
    real(kind=r_tran), dimension(undf_md), intent(in) :: reconstruction
    real(kind=r_tran), dimension(undf_ws), intent(inout) :: tracer
    real(kind=r_tran), dimension(undf_c), intent(in) :: coeff
    integer(kind=i_def), dimension(ndf_ws,cells_in_stencil), intent(in) :: stencil_map
    integer(kind=i_def) :: k
    integer(kind=i_def) :: f
    integer(kind=i_def) :: p
    integer(kind=i_def) :: id_r
    integer(kind=i_def) :: id_t
    integer(kind=i_def) :: id_c
    integer(kind=i_def) :: nl

    nl = ndf_ws + nlayers - 2
    do f = nfaces, 1, -1
      id_r = f * nl + f - nl + map_md(1) - 1
      do p = cells_in_stencil, 1, -1
        id_c = f * stencil_size + p - stencil_size + map_c(1) - 1
        id_t = stencil_map(1,p)
        do k = nl, 0, -1
          tracer(id_t + k) = tracer(id_t + k) + coeff(id_c) * reconstruction(id_r + k)
        enddo
      enddo
    enddo

  end subroutine adj_poly2d_reconstruction_lbl_code

end module adj_poly2d_reconstruction_lbl_kernel_mod
