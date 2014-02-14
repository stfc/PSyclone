! Copyright 2013 STFC, all rights reserved
program undeclared_function

  ! Description: function is not declared so we should expect an error
  ! ==> use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, m1, m2

  call invoke(                                                          &
       testkern_type(f1,f2,m1,m2)                                       &
          )

end program undeclared_function
