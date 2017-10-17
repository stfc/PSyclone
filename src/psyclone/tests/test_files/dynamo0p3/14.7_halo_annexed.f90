program single_invoke_annexed

  ! Description: f1 and f2 are written to by intrinsics and
  ! then read. f1 is on the w1 function space and f2 is on the w2
  ! function space, so both are continuous and therefore have annexed
  ! dofs. By default the intrinsics only write to owned
  ! dofs. Therefore a halo exchange will be required so that the
  ! annexed dofs for both f1 and f2 are clean when they are read.
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
