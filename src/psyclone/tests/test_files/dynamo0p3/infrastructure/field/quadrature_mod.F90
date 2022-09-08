!-----------------------------------------------------------------------------
! (C) Crown copyright 2017-2020 Met Office. All rights reserved.
! For further details please refer to the file LICENCE which you should have
! received as part of this distribution.
!-------------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-----------------------------------------------------------------------------
! Abstract base quadrature type.
!-------------------------------------------------------------------------------
!> @brief Abstract base type for for quadrature
module quadrature_mod
implicit none
private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, abstract :: quadrature_type
  private

end type

end module quadrature_mod
