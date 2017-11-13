!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program multikernel_invokes_3

  ! Multiple kernel calls within an invoke where the kernels require
  ! a quadrature rule

  use testkern_chi, only : testkern_chi_type
  use inf, only : field_type
  implicit none
  type(field_type) :: f1, chi(3), f2

  call invoke(                       &
       testkern_chi_type(f1,chi,f2), &
       testkern_chi_type(f1,chi,f2)  &
       )

end program multikernel_invokes_3
