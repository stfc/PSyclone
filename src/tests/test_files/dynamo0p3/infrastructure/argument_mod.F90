! Modifications copyright (c) 2017, Science and Technology Facilities Council
!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> @brief The argument type to hold kernel metadata required by the psy layer.

module argument_mod

  implicit none

! Function-space labels
  integer, public, parameter :: W0 = 1
  integer, public, parameter :: W1 = 2
  integer, public, parameter :: W2 = 3
  integer, public, parameter :: W3 = 4
  integer, public, parameter :: Wtheta = 5
  integer, public, parameter :: W2V = 6
  integer, public, parameter :: W2H = 7
  integer, public, parameter :: Wchi = 8

! argument types
  integer, public, parameter :: GH_FIELD    = 1 
  integer, public, parameter :: GH_OPERATOR = 2
  integer, public, parameter :: GH_REAL     = 3
  integer, public, parameter :: GH_INTEGER  = 4

! access descriptors
  integer, public, parameter :: GH_READ  = 11
  integer, public, parameter :: GH_WRITE = 12
  integer, public, parameter :: GH_RW    = 13
  integer, public, parameter :: GH_INC   = 14
  integer, public, parameter :: GH_SUM   = 15
  integer, public, parameter :: GH_MIN   = 16
  integer, public, parameter :: GH_MAX   = 17

! distinct any_space id's. Separate id's required as we may have groups of fields
! that must be on the same space within a kernel.
  integer, public, parameter :: ANY_SPACE_1  = 201
  integer, public, parameter :: ANY_SPACE_2  = 202
  integer, public, parameter :: ANY_SPACE_3  = 203
  integer, public, parameter :: ANY_SPACE_4  = 204
  integer, public, parameter :: ANY_SPACE_5  = 205
  integer, public, parameter :: ANY_SPACE_6  = 206
  integer, public, parameter :: ANY_SPACE_7  = 207
  integer, public, parameter :: ANY_SPACE_8  = 208
  integer, public, parameter :: ANY_SPACE_9  = 209
  integer, public, parameter :: ANY_SPACE_10 = 210

! function space attributes
  integer, public, parameter :: GH_BASIS       = 301 
  integer, public, parameter :: GH_DIFF_BASIS  = 302
  integer, public, parameter :: GH_ORIENTATION = 303

! kernel iterator
  integer, public, parameter :: CELLS     = 401
  integer, public, parameter :: ALL_DOFS  = 402

  type, public :: arg_type
     integer :: arg_type         ! {GH_FIELD, GH_OPERATOR, GH_REAL, GH_INTEGER}
     integer :: arg_intent       ! {GH_READ, GH_WRITE, GH_RW, GH_INC, GH_SUM, GH_MIN, GH_MAX}
     integer :: wspace      = -1 ! {W0, W1, W2, W3, ANY_SPACE_[0-9]+}
     integer :: from_wspace = -1 ! { " } only required for gh_operator
  end type arg_type

  type, public :: func_type
     integer :: wspace            ! {W0, W1, W2, W3, ANY_SPACE_[0-9]+}
     integer :: wproperties1      ! {GH_BASIS, GH_DIFF_BASIS, GH_OPERATOR}
     integer :: wproperties2 = -1 ! { " } optional and must be a distinct property
     integer :: wproperties3 = -1 ! { " } optional and must be a distinct property
  end type func_type

end module argument_mod

