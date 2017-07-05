program eval_invoke

  ! Test program containing a single invoke of a kernel that
  ! requires an evaluator
  use testkern_eval, only: testkern_eval_type
  implicit none
  type(field_type)      :: f0, f1
  type(quadrature_type) :: qr0

  call invoke(                           &
       testkern_eval_type(f0,f1,qr0),    &
       )


end program eval_invoke
