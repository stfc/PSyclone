program single_invoke_annexed

  ! Description: annexed halos example
  use testkern_w3, only: testkern_w3_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, m1, m2
  real(r_def) :: a

  call invoke(                         &
       setval_c(f1,0.0),               &
       setval_c(f2,0.0),               &
       testkern_w3_type(a,f1,f2,m1,m2) &
          )

end program single_invoke_annexed
