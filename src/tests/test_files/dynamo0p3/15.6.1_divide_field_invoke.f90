! Author A. R. Porter STFC Daresbury Lab

program single_invoke

  ! Description: single built-in operation (divide field)
  ! specified in an invoke call
  use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2

  call invoke( divide_field(f1, f2) )

end program single_invoke
