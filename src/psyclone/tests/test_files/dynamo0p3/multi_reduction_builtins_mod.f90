!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2015. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Modified I. Kavcic Met Office
!
!>@brief Meta-data for the Dynamo 0.3 built-in operations.
!>@details This meta-data is broken for testing purposes
module dynamo0p3_builtins_mod
  !> Fake built-in that purports to do two reductions
  type, public, extends(kernel_type) :: inner_product
     private
     type(arg_type) :: meta_args(3) = (/                              &
          arg_type(GH_REAL,  GH_SUM               ),                  &
          arg_type(GH_REAL,  GH_SUM               ),                  &
          arg_type(GH_FIELD, GH_WRITE, ANY_SPACE_1)                   &
          /)
     integer :: iterates_over = DOFS
   contains
     procedure, nopass :: setval_c_code
  end type inner_product

contains

  subroutine setval_c_code()
  end subroutine setval_c_code
  
end module dynamo0p3_builtins_mod
