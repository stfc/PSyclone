!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2015. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Modified I. Kavcic Met Office
!
!>@brief Meta-data for the Dynamo 0.3 built-in operations.
module dynamo0p3_builtins_mod
  !> Fake built-in that purports to do a reduction into an integer scalar
  type, public, extends(kernel_type) :: X_innerproduct_Y
     private
     type(arg_type) :: meta_args(2) = (/                            &
          arg_type(GH_INTEGER,   GH_SUM),                           &
          arg_type(GH_FIELD, GH_READ, ANY_SPACE_1)                  &
          /)
     integer :: iterates_over = DOFS
   contains
     procedure, nopass :: X_innerproduct_Y_code
  end type X_innerproduct_Y

contains

  subroutine X_innerproduct_Y_code()
  end subroutine X_innerproduct_Y_code
  
end module dynamo0p3_builtins_mod
