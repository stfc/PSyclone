!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern_chi_2

  use argument_mod
  use kernel_mod
  use constants_mod
  
  type, public, extends(kernel_type) :: testkern_chi_type
     type(arg_type), dimension(2) :: meta_args =    &
          (/ arg_type(gh_field*3,gh_read,w0),        &
             arg_type(gh_field,gh_write,w0)       &
          /)
     integer :: iterates_over = cells
   contains
     procedure, public, nopass :: code => testkern_code
  end type testkern_chi_type
contains

  subroutine testkern_code(nlayers, chi1, chi2, chi3, f1, &
                           ndf_w0, undf_w0, map_w0)
    implicit none
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w0, undf_w0
    real(kind=r_def), dimension(undf_w0),   intent(in)    :: chi1, chi2, chi3
    real(kind=r_def), dimension(undf_w0),   intent(inout) :: f1
    integer(kind=i_def), dimension(ndf_w0), intent(in)    :: map_w0
  end subroutine testkern_code
end module testkern_chi_2
