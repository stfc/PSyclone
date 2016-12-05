!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. R. Porter STFC Daresbury Lab

!> Test kernel that purports to write to both a field and an operator.
module testkern_write_op_and_fld
  type, extends(kernel_type) :: testkern_write_op_and_fld_type
     type(arg_type), dimension(3) :: meta_args =    &
          (/ arg_type(gh_field*3,gh_write,w3),      &
             arg_type(gh_integer,gh_read),          &
             arg_type(gh_operator,gh_write,w0,w0)   &
          /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_write_op_and_fld_code
  end type testkern_write_op_and_fld_type
contains
  subroutine testkern_write_op_and_fld_code()
  end subroutine testkern_write_op_and_fld_code
end module testkern_write_op_and_fld
