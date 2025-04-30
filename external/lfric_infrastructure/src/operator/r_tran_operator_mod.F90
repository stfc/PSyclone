!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief A module providing operator related classes.
!>
!> @details Implements the locally assembled operator (i.e. the stencil is
!>          assembled in each cell of the 3d grid)

module r_tran_operator_mod

  ! Eventually the precision of the operator will be set in a module held
  ! within the model (as it is model information). For now, PSyclone is
  ! expecting to "use" the definitions from operator_mod, so it is set here
#if (R_TRAN_PRECISION == 32)
  use operator_real32_mod, only: r_tran_operator_type => operator_real32_type, &
                                 r_tran_operator_proxy_type => operator_real32_proxy_type
#else
  use operator_real64_mod, only: r_tran_operator_type => operator_real64_type, &
                                 r_tran_operator_proxy_type => operator_real64_proxy_type
#endif

  implicit none
! Removing the following "private" statement is a workaround for a bug that
! appeared in Intel v19. Every item in the module has an explicit access set,
! so not setting the default has no effect. See ticket #3326 for details
!  private
  public :: r_tran_operator_type, r_tran_operator_proxy_type

end module r_tran_operator_mod
