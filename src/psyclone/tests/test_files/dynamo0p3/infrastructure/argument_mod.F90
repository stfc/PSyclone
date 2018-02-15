!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------

! BSD 3-Clause License
!
! Modifications copyright (c) 2017, Science and Technology Facilities Council
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
 
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
  integer, public, parameter :: ANY_W2 = 9

! argument types
  integer, public, parameter :: GH_FIELD    = 1 
  integer, public, parameter :: GH_OPERATOR = 2
  integer, public, parameter :: GH_COLUMNWISE_OPERATOR = 3
  integer, public, parameter :: GH_REAL     = 4
  integer, public, parameter :: GH_INTEGER  = 5

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
  integer, public, parameter :: GH_COLUMN_BANDED_DOFMAP = 304
  integer, public, parameter :: GH_COLUMN_INDIRECTION_DOFMAP = 305

! kernel iterator
  integer, public, parameter :: CELLS     = 401
  integer, public, parameter :: ALL_DOFS  = 402

! Quadrature metadata
  integer, public, parameter :: QUADRATURE_XYZ      = 501
  integer, public, parameter :: QUADRATURE_XYoZ     = 502
  integer, public, parameter :: QUADRATURE_XoYoZ    = 503
  integer, public, parameter :: GH_QUADRATURE_XYZ   = 504
  integer, public, parameter :: GH_QUADRATURE_XYoZ  = 505
  integer, public, parameter :: GH_QUADRATURE_XoYoZ = 506

! Evaluator metadata
  integer, public, parameter :: EVALUATOR           = 507
  integer, public, parameter :: GH_EVALUATOR        = 508

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

