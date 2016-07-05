!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. R. Porter STFC Daresbury Lab

program single_invoke

  ! Description: single point-wise set operation specified in an invoke call
  ! with the scalar value passed by reference
  ! Forbidden use statement for the built-in operation - built-ins do
  ! not have associated use statements.
  use fake_builtin_mod, only: set_field_scalar
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1
  real(r_def) :: fred

  fred = 20.1_r_def

  call invoke(                      &
       set_field_scalar(fred, f1)   &
          )

end program single_invoke
