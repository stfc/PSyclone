# 1 "field/kernel_mod.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "field/kernel_mod.F90"
!-----------------------------------------------------------------------------
! Copyright (c) 2017-2020,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------

! Abstract base kernel type.
!-------------------------------------------------------------------------------
!> @brief Abstract base type for for kernels
module kernel_mod
implicit none
private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, abstract :: kernel_type
  private

end type

!-------------------------------------------------------------------------------
! Interfaces
!-------------------------------------------------------------------------------

end module kernel_mod
