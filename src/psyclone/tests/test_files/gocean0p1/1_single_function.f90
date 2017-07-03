! Copyright 2013 STFC, all rights reserved
program single_function

  ! Description: single function specified in an invoke call
  use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, m1

  call invoke(                   &
       testkern_type(f1,f2,m1)   &
          )

end program single_function
