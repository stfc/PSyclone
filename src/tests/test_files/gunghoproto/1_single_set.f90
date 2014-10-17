! Copyright 2013 STFC, all rights reserved
program single_set

  ! Description: single infrastructure set routine specified in an invoke call
  use inf,      only: field_type
  implicit none
  type(field_type) :: one

  call invoke(                                           &
       set(one,1.0)                                      &
       )

end program single_set
