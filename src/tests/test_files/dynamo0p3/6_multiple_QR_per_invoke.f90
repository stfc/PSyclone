program multi_qr_per_invoke

  use testkern_qr, only: testkern_qr_type
  implicit none
  type(field_type)      :: f0, f1, f2, f3, f4
  type(quadrature_type) :: qr0, qr1

  call invoke(                               &
       testkern_qr_type(f1,f2,f3,f4,qr0),    &
       testkern_qr_type(f1,f2,f3,f4,qr1),    &
       testkern_qr_type(f0,f2,f3,f4,qr0)     &
       )


end program multi_qr_per_invoke
