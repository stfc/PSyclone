module adj_poly1d_reconstruction_kernel_mod
  use argument_mod, only : any_discontinuous_space_1, any_discontinuous_space_2, any_discontinuous_space_3, arg_type, cell_column, &
&cross, func_type, gh_field, gh_integer, gh_read, gh_real, gh_scalar, gh_write, reference_element_data_type, stencil
  use constants_mod, only : i_def, r_tran
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_poly1d_reconstruction_kernel_type
  type(ARG_TYPE) :: META_ARGS(5) = (/ &
    arg_type(gh_field, gh_real, gh_write, any_discontinuous_space_1), &
    arg_type(gh_field, gh_real, gh_write, any_discontinuous_space_2, stencil(cross)), &
    arg_type(gh_field, gh_real, gh_read, any_discontinuous_space_3), &
    arg_type(gh_scalar, gh_integer, gh_read), &
    arg_type(gh_scalar, gh_integer, gh_read)/)
  INTEGER :: OPERATES_ON = cell_column
  CONTAINS
    PROCEDURE, NOPASS :: adj_poly1d_reconstruction_code
END TYPE adj_poly1d_reconstruction_kernel_type

  private

  public :: adj_poly1d_reconstruction_code

  contains
  subroutine adj_poly1d_reconstruction_code(nlayers, reconstruction, tracer, stencil_size, stencil_map, coeff, ndata, order, &
&ndf_md, undf_md, map_md, ndf_ws, undf_ws, map_ws, ndf_c, undf_c, map_c)
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
    integer(kind=i_def), intent(in) :: order
    integer(kind=i_def), intent(in) :: stencil_size
    real(kind=r_tran), dimension(undf_md), intent(out) :: reconstruction
    real(kind=r_tran), dimension(undf_ws), intent(out) :: tracer
    real(kind=r_tran), dimension(undf_c), intent(in) :: coeff
    integer(kind=i_def), dimension(ndf_ws,stencil_size), intent(in) :: stencil_map
    integer(kind=i_def) :: k
    integer(kind=i_def) :: f
    integer(kind=i_def) :: p
    integer(kind=i_def) :: face
    integer(kind=i_def) :: stencil
    integer(kind=i_def) :: stencil_depth
    integer(kind=i_def) :: depth
    integer(kind=i_def) :: face_mod
    integer(kind=i_def) :: ijp
    integer(kind=i_def) :: df
    integer(kind=i_def) :: nl
    INTEGER(KIND = i_def), DIMENSION(order + 1, nfaces) :: map1d
    integer :: idx

    nl = ndf_ws + nlayers - 2
    stencil_depth = order / 2
    map1d(1,:) = 1
    do face = 1, nfaces, 1
      depth = 1
      face_mod = stencil_depth * MOD(face + 1, 2)
      do stencil = 2, stencil_depth + 1, 1
        map1d(stencil + depth - 1,face) = face_mod + stencil
        map1d(stencil + depth,face) = face_mod + order + stencil
        depth = depth + 1
      enddo
    enddo
    map1d(stencil + depth,face) = order * MOD(stencil, 2) + stencil + stencil_depth * MOD(face + 1, 2)
    do p = 1, order + 1, 1
       do f = 1, nfaces, 1
          if(map1d(p,f) /= 1)then
             ! Off-diagonal term
            call adj_compute_reconstruction(p, f, order, &
                                            reconstruction, stencil_map(1,map1d(p,f)), &
                                            coeff, map_c(1), tracer, map_md(1))
         else
            ! Diagonal term (update owned cell only)
            call adj_compute_reconstruction(p, f, order, &
                                            reconstruction, map_md(1,cell), &
                                            coeff, map_c(1,cell), &
                                            tracer, stencil_map(1,map1d(p,f),cell))

         end if
      enddo
    enddo
    do f = nfaces, 1, -1
      df = f * nl + f - nl + map_md(1) - 1
      do idx = df + nl, df, -1
        reconstruction(idx) = 0.0
      enddo
    enddo

  end subroutine adj_poly1d_reconstruction_code
  subroutine adj_compute_reconstruction(p, f, order, reconstruction, recon_cell, coeff, coeff_cell, tracer, tracer_cell, nl)
    integer, intent(in) :: p
    integer, intent(in) :: f
    integer, intent(in) :: nl
    integer, intent(in) :: order
    integer, intent(in) :: recon_cell
    integer, intent(in) :: coeff_cell
    integer, intent(in) :: tracer_cell
    real(kind=r_tran), dimension(:), intent(in) :: reconstruction
    real(kind=r_tran), dimension(:), intent(inout) :: tracer
    real(kind=r_tran), dimension(:), intent(in) :: coeff
    integer :: k
    integer :: ijp
    integer :: df
    integer :: stencil_depth
    integer :: face_mod
    integer :: face
    integer :: depth

    df = f * nl + f - nl + recon_cell - 1
    ijp = coeff_cell + f * order + f - order + p - 2
    do k = nl, 0, -1
      tracer(k + tracer_cell) = tracer(k + tracer_cell) + coeff(ijp) * reconstruction(df + k)
    enddo

  end subroutine adj_compute_reconstruction

end module adj_poly1d_reconstruction_kernel_mod
