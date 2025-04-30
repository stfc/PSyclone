!-----------------------------------------------------------------------------
! (C) Crown copyright 2024 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief A module providing scalar related classes.
!>
!> @details A representation of an integer scalar which provides both easy
!> access to the scalar data and a method by which the PSy layer can access
!> the distributed memory aspects of the scalar


module integer_scalar_mod

  ! Eventually the precision of the scalar will be set in a module held
  ! within the model (as it is model information). For now, PSyclone is
  ! expecting to "use" the definitions from scalar_mod, so it is set here

  use scalar_int32_mod, only: integer_scalar_type => scalar_int32_type

  implicit none
  private

  public :: integer_scalar_type

end module integer_scalar_mod
