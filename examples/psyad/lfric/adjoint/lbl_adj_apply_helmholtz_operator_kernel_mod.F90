module adj_apply_helmholtz_operator_kernel_mod
  use argument_mod, only : arg_type, cell_column, cross2d, gh_field, gh_logical, gh_read, gh_readwrite, gh_real, gh_scalar, &
&gh_write, stencil
  use constants_mod, only : i_def, l_def, r_solver
  use fs_continuity_mod, only : w3
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_apply_helmholtz_operator_kernel_type
  type(ARG_TYPE) :: META_ARGS(4) = (/ &
    arg_type(gh_field, gh_real, gh_readwrite, w3), &
    arg_type(gh_field, gh_real, gh_readwrite, w3, stencil(cross2d)), &
    arg_type(gh_field*9, gh_real, gh_read, w3), &
    arg_type(gh_scalar, gh_logical, gh_read)/)
  INTEGER :: OPERATES_ON = cell_column
  CONTAINS
    PROCEDURE, NOPASS :: adj_apply_helmholtz_operator_code
END TYPE adj_apply_helmholtz_operator_kernel_type

  private

  public :: adj_apply_helmholtz_operator_code

  contains
  subroutine adj_apply_helmholtz_operator_code(nlayers, y, x, smap_sizes, max_length, smap, helm_c, helm_n, helm_e, helm_s, &
&helm_w, helm_u, helm_uu, helm_d, helm_dd, limited_area, ndf, undf, map)
    integer(kind=i_def), intent(in) :: ndf
    integer(kind=i_def), intent(in) :: undf
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: max_length
    logical(kind=l_def), intent(in) :: limited_area
    integer(kind=i_def), dimension(4), intent(in) :: smap_sizes
    integer(kind=i_def), dimension(ndf), intent(in) :: map
    integer(kind=i_def), dimension(ndf,max_length,4), intent(in) :: smap
    real(kind=r_solver), dimension(undf), intent(inout) :: y
    real(kind=r_solver), dimension(undf), intent(inout) :: x
    real(kind=r_solver), dimension(undf), intent(in) :: helm_c
    real(kind=r_solver), dimension(undf), intent(in) :: helm_n
    real(kind=r_solver), dimension(undf), intent(in) :: helm_e
    real(kind=r_solver), dimension(undf), intent(in) :: helm_s
    real(kind=r_solver), dimension(undf), intent(in) :: helm_w
    real(kind=r_solver), dimension(undf), intent(in) :: helm_u
    real(kind=r_solver), dimension(undf), intent(in) :: helm_uu
    real(kind=r_solver), dimension(undf), intent(in) :: helm_d
    real(kind=r_solver), dimension(undf), intent(in) :: helm_dd
    integer(kind=i_def) :: k
    integer(kind=i_def) :: branch
    integer(kind=i_def) :: cell
    real(kind=r_solver), dimension(max_length,4) :: coeff

    k = nlayers - 2
    k = 1
    do k = nlayers - 1, 2, -1
      x(k + map(1) - 1) = x(k + map(1) - 1) + helm_d(k + map(1)) * y(map(1) + k)
      x(k + map(1) - 2) = x(k + map(1) - 2) + helm_dd(k + map(1)) * y(map(1) + k)
    enddo
    x(k + map(1) - 1) = x(k + map(1) - 1) + helm_d(k + map(1)) * y(map(1) + k)
    x(k + map(1) + 1) = x(k + map(1) + 1) + helm_u(k + map(1)) * y(map(1) + k)
    do k = nlayers - 3, 0, -1
      x(k + map(1) + 1) = x(k + map(1) + 1) + helm_u(k + map(1)) * y(map(1) + k)
      x(k + map(1) + 2) = x(k + map(1) + 2) + helm_uu(k + map(1)) * y(map(1) + k)
    enddo
    if (limited_area) then
      coeff(1,:) = 0.0_r_solver
      do k = nlayers - 1, 0, -1
        coeff(2,1) = helm_w(map(1) + k)
        coeff(2,2) = helm_s(map(1) + k)
        coeff(2,3) = helm_e(map(1) + k)
        coeff(2,4) = helm_n(map(1) + k)
        do branch = 4, 1, -1
          do cell = smap_sizes(branch), 1, -1
            x(k + smap(1,cell,branch)) = x(k + smap(1,cell,branch)) + coeff(cell,branch) * y(map(1) + k)
          enddo
        enddo
        x(k + smap(1,1,1)) = x(k + smap(1,1,1)) + helm_c(k + map(1)) * y(map(1) + k)
        y(map(1) + k) = 0.0
      enddo
    else
      do k = nlayers - 1, 0, -1
        x(k + smap(1,1,1)) = x(k + smap(1,1,1)) + helm_c(k + map(1)) * y(map(1) + k)
        x(k + smap(1,2,3)) = x(k + smap(1,2,3)) + helm_e(k + map(1)) * y(map(1) + k)
        x(k + smap(1,2,4)) = x(k + smap(1,2,4)) + helm_n(k + map(1)) * y(map(1) + k)
        x(k + smap(1,2,2)) = x(k + smap(1,2,2)) + helm_s(k + map(1)) * y(map(1) + k)
        x(k + smap(1,2,1)) = x(k + smap(1,2,1)) + helm_w(k + map(1)) * y(map(1) + k)
        y(map(1) + k) = 0.0
      enddo
    end if

  end subroutine adj_apply_helmholtz_operator_code

end module adj_apply_helmholtz_operator_kernel_mod
