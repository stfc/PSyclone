!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern_chi_mod
  type, extends(kernel_type) :: testkern_chi_type
     type(arg_type), dimension(3) :: meta_args =    &
          (/ arg_type(gh_field,gh_write,w0),        &
             arg_type(gh_field*3,gh_write,w0),      &
             arg_type(gh_field,gh_read,w0)          &
          /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_code
  end type testkern_chi_type
contains

  subroutine testkern_code()
  end subroutine testkern_code
end module testkern_chi_mod
