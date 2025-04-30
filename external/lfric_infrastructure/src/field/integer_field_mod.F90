!-----------------------------------------------------------------------------
! (C) Crown copyright 2024 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief A module providing integer field related classes.
!>
!> @details This is a version of a field object that can hold integer data
!> values. It contains both a representation of an integer field which provides
!> no access to the underlying data (to be used in the algorithm layer) and an
!> accessor class (to be used in the Psy layer).

module integer_field_mod

  use field_int32_mod, only: integer_field_type         => field_int32_type, &
                             integer_field_proxy_type   => field_int32_proxy_type, &
                             integer_field_pointer_type => field_int32_pointer_type

  implicit none
  private

  public :: integer_field_type, &
            integer_field_proxy_type, &
            integer_field_pointer_type

end module integer_field_mod
