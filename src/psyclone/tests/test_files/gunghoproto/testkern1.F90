!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern1
  type, extends(kernel_type) :: testkern1_type
     type(arg), dimension(4) :: meta_args =    &
          (/ arg(READ,  (CG(1)*CG(1)),    FE), &
             arg(READ,  (DG(0)*DG(0)),    FE), &
             arg(WRITE, (CG(1)*CG(1))**3, FE), &
             arg(WRITE, R,                FE)  &
           /)
     integer, parameter :: iterates_over = CELLS
   contains
     procedure() :: code => testkern1_code
  end type testkern1_type
contains
  subroutine testkern1_code(a,b,c,d)
  end subroutine testkern1_code
end module testkern1
