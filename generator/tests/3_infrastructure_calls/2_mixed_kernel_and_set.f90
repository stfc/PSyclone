! Copyright 2013 STFC, all rights reserved
program kernel_and_inf_mixed_invoke

  ! Description: single infrastructure set routine specified in an invoke call
  use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: one,f2,f3

  call invoke(                                           &
       set(one,1.0),                                     &
       testkern_type(one,f2,f3)                          &
       )

end program kernel_and_inf_mixed_invoke
