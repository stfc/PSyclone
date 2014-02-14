module testkern
  type, extends(kernel_type) :: testkern_type
     type(arg), dimension(3) :: meta_args =    &
          (/ arg(READ,  (CG(1)*CG(1)),    FE), &
             arg(READ,  (DG(0)*DG(0)),    FE), &
             arg(WRITE, (CG(1)*CG(1))**3, FE)  &
           /)
     integer, parameter :: iterates_over = CELLS
   contains
     procedure() :: code => testkern_code
  end type testkern_type
contains
  subroutine testkern_code(a,b,c)
  end subroutine testkern_code
end module testkern
