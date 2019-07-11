!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern_two_real_scalars
  use argument_mod
  use kernel_mod
  use constants_mod
  
  type, extends(kernel_type) :: testkern_type
     type(arg_type), dimension(6) :: meta_args = &
          (/ arg_type(gh_real,  gh_read    ), &
             arg_type(gh_field, gh_write,w1), &
             arg_type(gh_field, gh_read, w2), &
             arg_type(gh_field, gh_read, w2), &
             arg_type(gh_field, gh_read, w3), &
             arg_type(gh_real,  gh_read    )  &
           /)
     integer :: iterates_over = cells
   contains
     procedure, public, nopass :: code => testkern_code
  end type testkern_type
contains

  subroutine testkern_code(nlayers, a, f1data, f2data, f3data, f4data, b, &
                           ndf_w1, undf_w1, map_w1,                       &
                           ndf_w2, undf_w2, map_w2,                       &
                           ndf_w3, undf_w3, map_w3 )
    implicit none
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1, ndf_w2, ndf_w3
    integer(kind=i_def), intent(in) :: undf_w1, undf_w2, undf_w3
    real(kind=r_def), dimension(undf_w1),   intent(inout) :: f1data
    real(kind=r_def), dimension(undf_w2),   intent(in)    :: f2data
    real(kind=r_def), dimension(undf_w2),   intent(in)    :: f3data
    real(kind=r_def), dimension(undf_w3),   intent(in)    :: f4data
    real(kind=r_def),                       intent(in)    :: a, b
    integer(kind=i_def), dimension(ndf_w1), intent(in)    :: map_w1
    integer(kind=i_def), dimension(ndf_w2), intent(in)    :: map_w2
    integer(kind=i_def), dimension(ndf_w3), intent(in)    :: map_w3    
  end subroutine testkern_code
  
end module testkern_two_real_scalars
