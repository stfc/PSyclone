!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. Porter STFC Daresbury Lab
! Funded by the GOcean project

subroutine const_parameter()

  ! Fake Fortran program for testing aspects of
  ! the PSyclone code generation system.

  use field_mod, only : r2d_field
  use kernel_scalar_float, only: bc_ssh
  implicit none

  type(r2d_field) :: p_fld

  call invoke( bc_ssh(0, p_fld) )

END subroutine CONST_PARAMETER

