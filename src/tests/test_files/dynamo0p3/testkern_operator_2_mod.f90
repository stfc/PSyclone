!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern_operator_2_mod
  type, extends(kernel_type) :: testkern_operator_2_type
     type(arg_type), dimension(1) :: meta_args =   &
          (/ arg_type(gh_operator,gh_write,w2,w3) /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_operator_2_code
  end type testkern_operator_2_type
contains
  subroutine testkern_operator_2_code()
  end subroutine testkern_operator_2_code
end module testkern_operator_2_mod
