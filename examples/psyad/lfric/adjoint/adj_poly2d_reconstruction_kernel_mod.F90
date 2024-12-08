module adj_poly2d_reconstruction_kernel_mod
  use argument_mod, only : any_discontinuous_space_1, any_discontinuous_space_2, any_discontinuous_space_3, arg_type, cell_column, &
       func_type, gh_field, gh_integer, gh_read, gh_readwrite, &
       gh_real, gh_scalar, gh_write, region, stencil
  use constants_mod, only : i_def, l_def, r_tran
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_poly2d_reconstruction_kernel_type
  type(ARG_TYPE) :: META_ARGS(5) = (/ &
    arg_type(gh_field, gh_real, gh_read, any_discontinuous_space_1, stencil(region)), &
    arg_type(gh_field, gh_real, gh_readwrite, any_discontinuous_space_2), &
    arg_type(gh_field, gh_real, gh_read, any_discontinuous_space_3, stencil(region)), &
    arg_type(gh_scalar, gh_integer, gh_read), &
    arg_type(gh_scalar, gh_integer, gh_read)/)
  INTEGER :: OPERATES_ON = cell_column
  CONTAINS
    PROCEDURE, NOPASS :: adj_poly2d_reconstruction_code
END TYPE adj_poly2d_reconstruction_kernel_type

  private

  public :: adj_poly2d_reconstruction_code

  contains
    subroutine adj_poly2d_reconstruction_code(nlayers, &
         reconstruction, cells_in_rstencil, rstencil_map, tracer, &
         coeff, cells_in_cstencil, cstencil_map, ndata, stencil_size, &
         ndf_md, undf_md, map_md, &
         ndf_ws, undf_ws, map_ws, ndf_c, undf_c, map_c)
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
    integer(kind=i_def), intent(in) :: cells_in_rstencil, cells_in_cstencil
    integer(kind=i_def), intent(in) :: stencil_size
    real(kind=r_tran), dimension(undf_md), intent(out) :: reconstruction
    real(kind=r_tran), dimension(undf_ws), intent(out) :: tracer
    real(kind=r_tran), dimension(undf_c), intent(in) :: coeff
    integer(kind=i_def), dimension(ndf_md,cells_in_rstencil), intent(in) :: rstencil_map
    integer(kind=i_def), dimension(ndf_c,cells_in_cstencil), intent(in) :: cstencil_map
    integer(kind=i_def) :: p
    integer(kind=i_def) :: nl

    ! A region stencil is defined as (for depth 1):
    !>                |9|8|7|
    !> REGION -->     |2|1|6|
    !>                |3|4|5|
    ! So we invert this such that it is:
    !>                |5|4|3|
    !> REGION -->     |6|1|2|
    !>                |7|8|9|
    integer(kind=i_def), dimension(ndf_c, cells_in_cstencil) :: adj_cstencil_map, &
                                                                adj_rstencil_map
    adj_cstencil_map(:,1) = cstencil_map(:,1)
    adj_rstencil_map(:,1) = rstencil_map(:,1)
    do p = 2,5,1
       adj_cstencil_map(:,p) = cstencil_map(:,p+4)
       adj_cstencil_map(:,p+4) = cstencil_map(:,p)
       adj_rstencil_map(:,p) = rstencil_map(:,p+4)
       adj_rstencil_map(:,p+4) = rstencil_map(:,p)
    end do

    nl = ndf_ws + nlayers - 2
    do p = 1, cells_in_rstencil, 1
      if (p == 1) then
        ! Diagonal (owned-cell) update.
        call adj_compute_reconstruction( &
             p, nfaces, stencil_size, reconstruction, map_md(1), &
             ! tracer cell was 'stencil_map(1,p)', i.e. the current
             ! owned cell which is actually map_ws(1).
            coeff, map_c(1), tracer, map_ws(1), nl)
      else
         ! Off-diagonal update.
         ! Use reflexivity to swap cell indices.
        call adj_compute_reconstruction( &
            p, nfaces, stencil_size, reconstruction, adj_rstencil_map(1,p), &
            coeff, adj_cstencil_map(1,p), tracer, map_ws(1), nl)
      end if
    enddo

  end subroutine adj_poly2d_reconstruction_code
  subroutine adj_compute_reconstruction(p, nfaces, stencil_size, reconstruction, recon_cell, &
                                        coeff, coeff_cell, tracer, tracer_cell, nl)
    integer, intent(in) :: p
    integer, intent(in) :: nfaces
    integer, intent(in) :: stencil_size
    integer, intent(in) :: nl
    integer, intent(in) :: recon_cell
    integer, intent(in) :: coeff_cell
    integer, intent(in) :: tracer_cell
    real(kind=r_tran), dimension(:), intent(in) :: reconstruction
    real(kind=r_tran), dimension(:), intent(inout) :: tracer
    real(kind=r_tran), dimension(:), intent(in) :: coeff
    integer :: f
    integer :: k
    integer :: id_r
    integer :: id_c
    integer :: id_t

    do f = nfaces, 1, -1
      id_r = f * nl + f - nl + recon_cell - 1
      id_t = tracer_cell
      id_c = coeff_cell + (f-1) * stencil_size + p - 1
      write(*,*) "UPDATING ",id_t,"from",id_c, id_r
      do k = nl, 0, -1
        tracer(id_t + k) = tracer(id_t + k) + coeff(id_c) * reconstruction(id_r + k)
      enddo
    enddo

  end subroutine adj_compute_reconstruction

end module adj_poly2d_reconstruction_kernel_mod
