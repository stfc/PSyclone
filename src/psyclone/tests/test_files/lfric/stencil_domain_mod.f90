module stencil_domain_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: stencil_domain_type
     type(arg_type), dimension(4) :: meta_args =                    &
          (/ arg_type(gh_field, gh_real, gh_readwrite, w3),         &
             arg_type(gh_field, gh_real, gh_read, w3, stencil(cross2d)), &
             arg_type(gh_field, gh_real, gh_read, w3),               &
             arg_type(gh_field, gh_real, gh_read, w3)                &
           /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => stencil_domain_code
  end type stencil_domain_type

contains

  subroutine stencil_domain_code(a, b, b_st_size, b_max, b_st_dofmap, c, d)
    implicit none
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_2d
    real(kind=r_def), intent(inout) :: a(:)
    real(kind=r_def), intent(in) :: b(:,:)
    integer(kind=i_def), intent(in) :: b_st_size(:,:)
    integer(kind=i_def), intent(in) :: b_max
    integer(kind=i_def), intent(in) :: b_st_dofmap(:,:,:,:)
    real(kind=r_def), intent(in) :: c(:)
    real(kind=r_def), intent(in) :: d(:)
  end subroutine stencil_domain_code

end module stencil_domain_mod
