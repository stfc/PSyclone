!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program multikernel_invokes_6

  ! Multiple kernel calls within an invoke where the arguments are specified
  ! as any_space

  use testkern_any_space_2_mod, only : testkern_any_space_2_type
  use inf, only : field_type, operator_type, quadrature_rule
  implicit none
  type(field_type)      :: f1, f2, f3(3)
  type(operator_type)   :: op
  type(quadrature_rule) :: qr
  integer :: scalar

  call invoke(                                         &
       testkern_any_space_2_type(f1, f2, op, scalar),  &
       testkern_any_space_2_type(f2, f1, op, scalar)   &
       )

end program multikernel_invokes_6
