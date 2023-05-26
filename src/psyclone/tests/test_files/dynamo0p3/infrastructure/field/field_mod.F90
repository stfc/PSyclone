!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief A module providing field related classes.
!>
!> @details This is a version of a field object that can hold real data
!> values. It contains both a representation of an real field which provides
!> no access to the underlying data (to be used in the algorithm layer) and an
!> accessor class (to be used in the Psy layer).


module field_mod

  ! Eventually the precision of the field data will be set in a module held
  ! within the model (as it is model information). For now, PSyclone is
  ! expecting to "use" the definitions from field_mod, so it is set here
#if (RDEF_PRECISION == 32)
  use field_r32_mod, only: field_type         => field_r32_type, &
                           field_proxy_type   => field_r32_proxy_type, &
                           field_pointer_type => field_r32_pointer_type
#else
  use field_r64_mod, only: field_type         => field_r64_type, &
                           field_proxy_type   => field_r64_proxy_type, &
                           field_pointer_type => field_r64_pointer_type
#endif

  implicit none
  private

  public :: field_type, &
            field_proxy_type, &
            field_pointer_type

end module field_mod
