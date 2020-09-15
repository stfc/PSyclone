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
! Modifications copyright (c) 2017-2020, Science and Technology
! Facilities Council.
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
! Modified I. Kavcic, Met Office
!          A. R. Porter, STFC Daresbury Laboratory

!> @brief The argument type to hold kernel metadata required by the PSy layer.

module argument_mod

  implicit none

  private

  ! Argument types
  integer, public, parameter :: GH_FIELD               = 1
  integer, public, parameter :: GH_OPERATOR            = 2
  integer, public, parameter :: GH_COLUMNWISE_OPERATOR = 3
  integer, public, parameter :: GH_REAL                = 4
  integer, public, parameter :: GH_INTEGER             = 5

  ! Access descriptors
  integer, public, parameter :: GH_READ      = 11
  integer, public, parameter :: GH_WRITE     = 12
  integer, public, parameter :: GH_READWRITE = 13
  integer, public, parameter :: GH_INC       = 14
  integer, public, parameter :: GH_SUM       = 15
  integer, public, parameter :: GH_MIN       = 16
  integer, public, parameter :: GH_MAX       = 17

  ! General function space IDs. Distinct IDs required as we may
  ! have groups of fields that must be on the same space within
  ! a kernel.
  ! IDs for any space regardless of continuity.
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
  ! IDs for any vector W2-type space regardless of continuity
  ! (w2, w2h, w2v, w2broken but not w2*trace spaces of scalar
  ! functions). Issue #540 will resolve what W2* spaces should
  ! be included in ANY_W2 list and how they should be treated.
  integer, public, parameter :: ANY_W2       = 112
  ! IDs for any discontinuous space
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_1  = 251
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_2  = 252
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_3  = 253
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_4  = 254
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_5  = 255
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_6  = 256
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_7  = 257
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_8  = 258
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_9  = 259
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_10 = 260

  ! Function space attributes
  integer, public, parameter :: GH_BASIS                     = 301
  integer, public, parameter :: GH_DIFF_BASIS                = 302
  integer, public, parameter :: GH_ORIENTATION               = 303
  integer, public, parameter :: GH_COLUMN_BANDED_DOFMAP      = 304
  integer, public, parameter :: GH_COLUMN_INDIRECTION_DOFMAP = 305

  ! Kernel iterator
  ! TODO #870 remove CELLS
  integer, public, parameter :: CELLS       = 401
  integer, public, parameter :: CELL_COLUMN = 402
  integer, public, parameter :: ALL_DOFS    = 403
  integer, public, parameter :: DOF         = 404

  ! Quadrature metadata
  integer, public, parameter :: QUADRATURE_XYZ      = 501
  integer, public, parameter :: QUADRATURE_XYoZ     = 502
  integer, public, parameter :: QUADRATURE_XoYoZ    = 503
  integer, public, parameter :: GH_QUADRATURE_XYZ   = 504
  integer, public, parameter :: GH_QUADRATURE_XYoZ  = 505
  integer, public, parameter :: GH_QUADRATURE_XoYoZ = 506
  integer, public, parameter :: GH_QUADRATURE_FACE  = 507
  integer, public, parameter :: GH_QUADRATURE_EDGE  = 508

  ! Evaluator metadata
  integer, public, parameter :: EVALUATOR           = 509
  integer, public, parameter :: GH_EVALUATOR        = 510

  ! Stencil metadata
  integer, public, parameter :: XORY1D = 1
  integer, public, parameter :: X1D    = 2
  integer, public, parameter :: Y1D    = 3
  integer, public, parameter :: CROSS  = 4
  integer, public, parameter :: STENCIL(4) = -1

  ! For inter-grid kernels
  integer, public, parameter :: GH_FINE = 701
  integer, public, parameter :: GH_COARSE = 702

  ! Mesh properties
  integer, public, parameter :: adjacent_face = 533

  ! Reference-element properties
  integer, public, parameter :: normals_to_horizontal_faces = 171
  integer, public, parameter :: outward_normals_to_horizontal_faces = 007
  integer, public, parameter :: normals_to_vertical_faces = 172
  integer, public, parameter :: outward_normals_to_vertical_faces = 008
  integer, public, parameter :: normals_to_faces = 173
  integer, public, parameter :: outward_normals_to_faces = 009

  type, public :: arg_type
     integer :: arg_type         ! {GH_FIELD, GH_OPERATOR, GH_REAL, GH_INTEGER}
     integer :: arg_intent       ! {GH_READ, GH_WRITE, GH_READWRITE, GH_INC, GH_SUM, GH_MIN, GH_MAX}
     integer :: wspace      = -1 ! {W0, W1, W2, W3, ANY_SPACE_[0-9]+}
     integer :: from_wspace = -1 ! { " } only required for gh_operator
     integer :: mesh_arg    = -1 ! {GH_COARSE, GH_FINE} only for inter-grid kernels
  end type arg_type

  type, public :: func_type
     integer :: wspace            ! {W0, W1, W2, W3, ANY_SPACE_[0-9]+}
     integer :: wproperties1      ! {GH_BASIS, GH_DIFF_BASIS, GH_OPERATOR}
     integer :: wproperties2 = -1 ! { " } optional and must be a distinct property
     integer :: wproperties3 = -1 ! { " } optional and must be a distinct property
  end type func_type

  type, public :: mesh_data_type
    integer :: mesh_data_item ! {adjacent_face}
  end type mesh_data_type

  type, public :: reference_element_data_type
    ! {normals_to_<horizontal/vertical/all>_faces, &
    !  outward_normals_to_<horizontal/vertical/all>_faces}
    integer :: reference_element_data_item
  end type reference_element_data_type
  
end module argument_mod
