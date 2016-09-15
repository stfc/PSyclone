! Author A. R. Porter, STFC Daresbury Lab

program single_invoke

  ! Description: single function specified in an invoke call with one
  ! argument obtained by dereferencing a derived type. The 2nd and 3rd
  ! arguments should have different names in the PSy layer.
  use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, m1, m2
  real(r_def) :: a

  call invoke(                                   &
       testkern_type(a,f1_my_field,f1%my_field,m1,m2)   &
          )

end program single_invoke
