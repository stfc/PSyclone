!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program multikernel_invokes_5

  ! Multiple kernel calls within an invoke where the kernels require
  ! orientation information

  use testkern_operator_mod, only : testkern_operator_type
  use inf, only : field_type, quadrature_rule
  implicit none
  type(field_type)      :: f1(3)
  type(operator_type)   :: op
  type(quadrature_rule) :: qr

  call invoke(                             &
       testkern_operator_type(op, f1, qr), &
       testkern_operator_type(op, f1, qr)  &
       )

end program multikernel_invokes_5
