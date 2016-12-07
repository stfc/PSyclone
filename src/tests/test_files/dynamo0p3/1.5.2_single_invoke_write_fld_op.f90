!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program single_invoke_fs

  ! Description: single function that writes to both an operator and
  ! a field specified in an invoke call
  use testkern_write_op_and_fld, only: testkern_write_op_and_fld_type
  use inf,      only: field_type, operator_type
  implicit none
  type(field_type) :: f1(3)
  type(operator_type) :: op1
  integer :: an_int

  call invoke( testkern_write_op_and_fld_type(f1, an_int, op1) )

end program single_invoke_fs
