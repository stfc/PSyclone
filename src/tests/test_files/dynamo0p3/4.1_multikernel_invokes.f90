!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program multikernel_invokes_2

  ! Multiple kernel calls within an invoke where the kernels require
  ! a quadrature rule

  use testkern_qr, only : testkern_qr_type
  use inf,      only: field_type
  implicit none
  real(r_def) :: a
  integer :: istp
  type(field_type) :: f1, f2, f3, f4
  type(quadrature_rule_type) :: qr

  call invoke(                                  &
       testkern_qr_type(f1,f2,f3,a,f4,istp,qr), &
       testkern_qr_type(f1,f2,f3,a,f4,istp,qr)  &
       )

end program multikernel_invokes_2
