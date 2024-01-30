!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------

!> @brief Interface for the quadrature rule functor

!> @details This module contains the interface for the quadrature rule functor

module quadrature_rule_mod
use constants_mod, only: r_def, i_def

implicit none
private

!Public types
type, abstract, public :: quadrature_rule_type
  private
contains
  procedure(quadrature_rule_interface), deferred :: quadrature_rule
end type

!Interfaces
abstract interface
  function quadrature_rule_interface(self, nqp_1d)
    import                          :: r_def, i_def, quadrature_rule_type
    class(quadrature_rule_type)     :: self
    integer(kind=i_def), intent(in) :: nqp_1d
    real(kind=r_def)                :: quadrature_rule_interface(nqp_1d,2)
  end function quadrature_rule_interface
end interface

end module quadrature_rule_mod
