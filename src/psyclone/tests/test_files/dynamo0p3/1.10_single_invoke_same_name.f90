! Author R. Ford STFC Daresbury Lab

program single_invoke

  ! Description: single function specified in an invoke call with the same
  ! name (f1) being passed in twice. This should make PSyclone raise an
  ! exception
  use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, m1, m2
  real(r_def) :: a

  call invoke(                      &
       testkern_type(a,f1,f1,m1,m2) &
          )

end program single_invoke
