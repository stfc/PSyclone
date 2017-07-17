!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern
  type, extends(kernel_type) :: testkern_type
     type(arg_type), dimension(3) :: meta_args =    &
          (/ arg_type(gh_rw,v1,fe,.false.,.false.,.false.,.false.), &
             arg_type(gh_rw,v2,fe,.false.,.false.,.false.,.false.), &
             arg_type(gh_rw,v3,fe,.false.,.false.,.false.,.false.)  &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_code
  end type testkern_type
contains

  subroutine testkern_code(a,b,c)
  end subroutine testkern_code
end module testkern
