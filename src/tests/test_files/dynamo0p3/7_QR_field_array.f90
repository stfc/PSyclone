program qr_field_array

  use testkern_qr, only: testkern_qr_type
  implicit none
  type(field_type)      :: f0, f1, f2, f3, f4
  type(quadrature_type) :: qr0(2,2), qr1(2,2)
  integer :: i, j, k(2), l

  call invoke(                                    &
       testkern_qr_type(f1,f2,f3,f4,qr0(i,j)),    &
       testkern_qr_type(f1,f2,f3,f4,qr0(i,j+1)),  &
       testkern_qr_type(f1,f2,f3,f4,qr1(i,k(l))), &
       testkern_qr_type(f0,f2,f3,f4,qr0(i,j))     &
       )


end program qr_field_array
