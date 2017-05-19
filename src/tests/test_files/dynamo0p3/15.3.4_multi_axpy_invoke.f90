!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. R. Porter STFC Daresbury Lab

program single_invoke

  ! Description: multi axpy point-wise operations specified in an invoke call
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2(7), f3
  real(r_def) :: a

  a = 0.5

  call invoke(                          &
              axpy(a, f1, f3, f2(1)),   &
              axpy(a, f1, f3, f2(2)),   &
              axpy(a, f1, f3, f2(3)),   &
              axpy(a, f1, f2(4), f3),   &
              axpy(a, f1, f3, f2(5)),   &
              axpy(a, f1, f3, f2(6)),   &
              axpy(a, f1, f3, f2(7))    &
             )

end program single_invoke
