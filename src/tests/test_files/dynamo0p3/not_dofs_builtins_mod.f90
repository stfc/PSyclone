!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2015. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!>@brief Meta-data for the Dynamo 0.3 built-in operations.
!>@details This meta-data is broken for testing purposes
module dynamo0p3_builtins_mod

  !> field1 = ascalar
  type, public, extends(kernel_type) :: set_field_scalar
     private
     type(arg_type) :: meta_args(2) = (/                              &
          arg_type(GH_REAL,  GH_READ),                                &
          arg_type(GH_FIELD, GH_WRITE, ANY_SPACE_1)                   &
          /)
     ! Deliberately BREAK the meta-data - we only support iterates_over
     ! DOFS for built-ins in the dynamo 0.3 API
     integer :: iterates_over = CELLS
   contains
     procedure, nopass :: set_field_scalar_code
  end type set_field_scalar

contains

  subroutine set_field_scalar_code()
  end subroutine set_field_scalar_code
  
end module dynamo0p3_builtins_mod
