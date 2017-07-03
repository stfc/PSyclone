!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program multikernel_invokes_452

  ! Multiple kernel calls within an invoke where the arguments are specified
  ! as any_space

  use testkern_any_space_1_mod, only : testkern_any_space_1_type
  use testkern_any_space_2_mod, only : testkern_any_space_2_type
  use testkern_any_space_3_mod, only : testkern_any_space_3_type
  use testkern_any_space_4_mod, only : testkern_any_space_4_type
  use inf, only : field_type, operator_type, quadrature_rule
  implicit none
  type(field_type)      :: f1, f2, f3(3)
  type(operator_type)   :: op, op2, op3, op4, op5
  type(quadrature_rule) :: qr
  integer               :: scalar
  real(r_def)           :: rdt

  call invoke(                                             &
       ! any1, any2, W0
       testkern_any_space_1_type(f1, rdt, f2, f3, qr),     &
       ! any1, any1, any1-any1
       testkern_any_space_2_type(f1, f2, op, scalar),      &
       ! any1, any1, any1-any1
       testkern_any_space_2_type(f2, f1, op, scalar),      &
       ! any1-any2
       testkern_any_space_3_type(op),                      &
       ! any5, any1-any2, any3-any2, any4-any4, any3-any5, any4
       testkern_any_space_4_type(f2,op2,op3,op4,op5,f1,qr) &
       )

end program multikernel_invokes_452
