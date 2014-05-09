!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> @mainpage Dynamo
!> PsyKAl is the architecture for Gung Ho. Whlist the computational and optimisation
!> infrastructure is being developed, the science code is being developed using 
!> a hand-rolled Psy layer, Psy-lite. A PsyKAl-lite needs a dynamo!
!> Eventually, PsyKAlite is replaced with the real Psy and Dynamo becomes Gung Ho.

!> @brief Main program used to illustrate dynamo functionality.

!> @details Creates the function spaces and alls the set_up to populate them (
!> either read or compute) then individual calls to the psy-layer with kernels
!> as if the code has been pre-processed by Psyclone.
!> Comments starting with !PSY are what the code would lool like before Psyclone
!> generated anything.



program dynamo
  use lfric
  use v3_kernel_mod,           only: v3_kernel_type
  use v3_solver_kernel_mod,    only: v3_solver_kernel_type
  !use psy,                     only: invoke_rhs_v3, invoke_v3_solver_kernel
  use set_up_mod,              only: set_up
  implicit none

  type(function_space_type)      :: v3_function_space, v2_function_space, & 
                                    v1_function_space, v0_function_space
  type(field_type)               :: pressure_density,rhs
  type(gaussian_quadrature_type) :: gq

  integer :: cell
  integer :: num_cells,num_layers,element_order
  
  write(*,'("Dynamo:Hello, World")') 

  call set_up(v0_function_space,v1_function_space,v2_function_space,      &
  v3_function_space, num_layers)

  gq = gaussian_quadrature_type()

  pressure_density = field_type(vector_space = v3_function_space,         &
       gq = gq,                                                           &
       num_layers = num_layers)

  rhs = field_type(vector_space = v3_function_space,                      &
       gq = gq,                                                           &
       num_layers = num_layers)

  !Construct PSy layer given a list of kernels. This is the line the code
  !generator may parse and do its stuff.

  write(*,*) "dynamo:calling 1st kernel"
  call invoke (v3_kernel_type(rhs) )
  !call invoke_rhs_v3(rhs)

  write(*,*) "dynamo:calling 2nd kernel"
  call invoke (v3_solver_kernel_type(pressure_density,rhs) )
  !call invoke_v3_solver_kernel(pressure_density,rhs)

  write(*,*) 'RHS field = '
  call print_field(rhs)
  write(*,*) 'LHS field = '
  call print_field(pressure_density)

end program dynamo

subroutine print_field(field)
! Subroutine to print field to screen
use lfric

implicit none

type(field_type), intent(in)  :: field

integer :: cell, layer, df
integer, pointer :: map(:)

do cell=1,field%vspace%get_ncell()
  call field%vspace%get_cell_dofmap(cell,map)
  do df=1,field%vspace%get_ndf()
    do layer=0,field%get_nlayers()-1
      write(*,*) cell,df,layer+1,field%data(map(df)+layer)
    end do
  end do
end do

end subroutine print_field
