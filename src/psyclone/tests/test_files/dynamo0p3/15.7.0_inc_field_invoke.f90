! Author A. R. Porter STFC Daresbury Lab

program single_invoke

  ! Description: single built-in operation (increment field)
  ! specified in an invoke call
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2

  call invoke( inc_field(f1, f2) )

end program single_invoke
