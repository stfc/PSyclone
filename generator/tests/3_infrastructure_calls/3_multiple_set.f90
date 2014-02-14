! Copyright 2013 STFC, all rights reserved
program multiple_set

  ! Description: single infrastructure set routine specified in an invoke call
  use inf,      only: field_type
  implicit none
  type(field_type) :: one,two

  call invoke(                                &
       set(one,1.0),                          &
       set(two,2.0)                           &
       )

end program multiple_set
