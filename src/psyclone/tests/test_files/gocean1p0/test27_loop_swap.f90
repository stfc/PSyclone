!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. Porter STFC Daresbury Lab
! Funded by the GOcean project

SUBROUTINE test27_loop_swap()

  use field_mod
  use boundary_conditions_ne_offset_mod, only : bc_ssh, bc_solid_u, bc_solid_v
  implicit none

  type(r2d_field) :: t, u, v
  ! Those three functions all create different i/j loop boundaries
  ! which simplifies testing.
  call invoke( name="loop1",  &
       bc_ssh(1, t),          &
       bc_solid_u(u),         &
       bc_solid_v(v)            )

  call invoke( name="loop2",  &
       bc_ssh(1, t),          &
       bc_ssh(1, t)             )
END SUBROUTINE test27_loop_swap
