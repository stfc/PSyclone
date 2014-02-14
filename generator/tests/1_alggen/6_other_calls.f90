! Copyright 2013 STFC, all rights reserved
program other_calls

  ! Description: include non-gungho calls
  use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, m1

  call init(f1,f2)

  call invoke(                                                          &
       testkern_type(f1,f2,m1)                                       &
          )

  call finalise(m1)

end program other_calls
