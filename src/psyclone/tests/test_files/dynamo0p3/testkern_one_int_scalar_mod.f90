!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern_one_int_scalar_mod
  use argument_mod
  use kernel_mod
  use constants_mod
  
  type, public, extends(kernel_type) :: testkern_one_int_scalar_type
     private
     type(arg_type), dimension(5) :: meta_args = &
          (/ arg_type(gh_field,   gh_write,w1), &
             arg_type(gh_integer, gh_read    ), &
             arg_type(gh_field,   gh_read, w2), &
             arg_type(gh_field,   gh_read, w2), &
             arg_type(gh_field,   gh_read, w3)  &
           /)
     integer :: iterates_over = cells
   contains
     procedure, public, nopass :: code => testkern_code
  end type testkern_one_int_scalar_type
contains

  subroutine testkern_code(nlayers, afield1, iflag, afield2, afield3, afield4, &
       ndf_w1, undf_w1, map_w1, ndf_w2, undf_w2, map_w2,                       &
       ndf_w3, undf_w3, map_w3 )
    implicit none
    integer(kind=i_def),               intent(in)  :: nlayers 
    real(kind=r_def), dimension(:),    intent(out) :: afield1
    integer(kind=i_def),               intent(in)  :: iflag
    real(kind=r_def), dimension(:),    intent(in)  :: afield2, afield3, afield4
    integer(kind=i_def),               intent(in)  :: ndf_w1, undf_w1
    integer(kind=i_def), dimension(:), intent(in)  :: map_w1
    integer(kind=i_def),               intent(in)  :: ndf_w2, undf_w2
    integer(kind=i_def), dimension(:), intent(in)  :: map_w2
    integer(kind=i_def),               intent(in)  :: ndf_w3, undf_w3
    integer(kind=i_def), dimension(:), intent(in)  :: map_w3    
  end subroutine testkern_code

end module testkern_one_int_scalar_mod
