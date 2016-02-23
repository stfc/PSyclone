!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program single_invoke

  ! Description: A very long assignment statement with continuation characters
  use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, m1, m2
  real(r_def) :: a

  my_very_long_variable_name(my_very_long_lookup_name(my_very_long_index)) = &
  my_very_long_variable_name(my_very_long_lookup_name(my_very_long_index)) + &
  my_very_long_value_name

  call invoke(                      &
       testkern_type(a,f1,f2,m1,m2)   &
          )

end program single_invoke
