!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2015. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!>@brief Meta-data for the Dynamo 0.3 built-in operations.
module dynamo0p3_builtins_mod
  !> Fake built-in that purports to do a reduction into an integer scalar
  type, public, extends(kernel_type) :: inner_product
     private
     type(arg_type) :: meta_args(2) = (/                            &
          arg_type(GH_INTEGER,   GH_SUM),                           &
          arg_type(GH_FIELD, GH_READ, ANY_SPACE_1)                  &
          /)
     integer :: iterates_over = DOFS
   contains
     procedure, nopass :: set_field_scalar_code
  end type inner_product

contains

  subroutine set_field_scalar_code()
  end subroutine set_field_scalar_code
  
end module dynamo0p3_builtins_mod
