! Author A. R. Porter STFC Daresbury Lab

program single_invoke

  ! Description: single point-wise operation (divide fields)
  ! specified in an invoke call
  use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, f3

  call invoke(                            &
              divide_fields(f1, f2, f3)   &
             )

end program single_invoke
