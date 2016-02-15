!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program multi_invoke_qr

  ! Description: single function specified in an invoke call
  use testkern_qr, only: testkern_qr_type
  use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, f3, m1, m2, m3
  type(quadrature_rule) :: qr
  real(r_def) :: a
  integer :: istp

  call invoke(                                  &
       testkern_qr_type(f1,f2,m1,a,m2,istp,qr), &
       testkern_type(a,f1,f2,m1,m3),            &
       testkern_qr_type(f1,f3,m1,a,m2,istp,qr)  &
       )

end program multi_invoke_qr
