!-----------------------------------------------------------------------------
! Copyright (c) 2017-2020,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2020-2021, Science and Technology
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
!
! Modified I. Kavcic and A. Coughtrie, Met Office
!          A. R. Porter, STFC Daresbury Laboratory


!> @brief The argument type to hold kernel metadata required by the PSy layer.

!> @details Metadata for the kernels. In order to create correct PSy code a
!> code we need to know how many fields and operators are passed to the kernel
!> and in what order they are passed. We also needs to know how these
!> fields/operators are accessed (read, write etc) within the kernel and what
!> function space they are on (w0, w1 etc). In the case of operators there are
!> two function spaces (to and from). This information is stored in the
!> arg_type type.

!> A kernel may also require additional data associated with a particular
!> function space (basis function and differential basis function
!> information). This information is specified in the xxx type.
!> the PSy layer needs to know how this is to be accessed.
!> read, write etc and which function space it belongs. These are the three
!> integers of
!> the arg_type and the values are then one of the parameters
!> defined in this module.
!> field metadata also has three logicals controlling whether the PSy layer
!> needs to pass the basis function, the differential basis function,
!> and the guassian quadrature type.
!> Another metadatum which describes the kernel, not the fields
!> is what the kernel will iterate over. Usually cells, sometimes
!> all the dofs.

module argument_mod

  implicit none

  private

! Argument types
  integer, public, parameter :: GH_FIELD               = 507
  integer, public, parameter :: GH_OPERATOR            = 735
  integer, public, parameter :: GH_COLUMNWISE_OPERATOR = 841
  integer, public, parameter :: GH_SCALAR              = 4

! Primitive Fortran types of argument data
  integer, public, parameter :: GH_REAL                = 58
  integer, public, parameter :: GH_INTEGER             = 5

! Access descriptors
  integer, public, parameter :: GH_READ      = 958
  integer, public, parameter :: GH_WRITE     = 65
  integer, public, parameter :: GH_READWRITE = 811
  integer, public, parameter :: GH_INC       = 542
  integer, public, parameter :: GH_SUM       = 563
  integer, public, parameter :: GH_MIN       = 718
  integer, public, parameter :: GH_MAX       = 391

! General function space IDs. Distinct IDs required as we may have groups
! of fields that must be on the same space within a kernel.
! IDs for any space regardless of continuity
  integer, public, parameter :: ANY_SPACE_1    = 368
  integer, public, parameter :: ANY_SPACE_2    = 389
  integer, public, parameter :: ANY_SPACE_3    = 194
  integer, public, parameter :: ANY_SPACE_4    = 816
  integer, public, parameter :: ANY_SPACE_5    = 461
  integer, public, parameter :: ANY_SPACE_6    = 734
  integer, public, parameter :: ANY_SPACE_7    = 890
  integer, public, parameter :: ANY_SPACE_8    = 74
  integer, public, parameter :: ANY_SPACE_9    = 922
  integer, public, parameter :: ANY_SPACE_10   = 790
! IDs for any vector W2-type space regardless of continuity
! (w2, w2h, w2v, w2broken but not w2*trace spaces of scalar
! functions). Issue #540 will resolve what W2* spaces should
! be included in ANY_W2 list and how they should be treated.
  integer, public, parameter :: ANY_W2         = 353
! IDs for any discontinuous space
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_1  = 43
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_2  = 711
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_3  = 267
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_4  = 901
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_5  = 174
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_6  = 683
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_7  = 425
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_8  = 361
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_9  = 536
  integer, public, parameter :: ANY_DISCONTINUOUS_SPACE_10 = 882

! Function space attributes
  integer, public, parameter :: GH_BASIS       = 751
  integer, public, parameter :: GH_DIFF_BASIS  = 767
  integer, public, parameter :: GH_COLUMN_BANDED_DOFMAP = 541
  integer, public, parameter :: GH_COLUMN_INDIRECTION_DOFMAP = 204


! Kernel iterator
! TODO #870 remove CELLS
  integer, public, parameter :: CELLS       = 396
  integer, public, parameter :: CELL_COLUMN = 402
  integer, public, parameter :: ALL_DOFS    = 945

! Quadrature metadata
  integer, public, parameter :: QUADRATURE_XYZ      = 501
  integer, public, parameter :: QUADRATURE_XYoZ     = 502
  integer, public, parameter :: QUADRATURE_XoYoZ    = 503
  integer, public, parameter :: GH_QUADRATURE_XYZ   = 912
  integer, public, parameter :: GH_QUADRATURE_XYoZ  = 849
  integer, public, parameter :: GH_QUADRATURE_XoYoZ = 701
  integer, public, parameter :: GH_QUADRATURE_FACE  = 539
  integer, public, parameter :: GH_QUADRATURE_EDGE  = 419

! Evaluator metadata
  integer, public, parameter :: EVALUATOR           = 959
  integer, public, parameter :: GH_EVALUATOR        = 510

! Coarse and fine function spaces
  integer, public, parameter :: GH_FINE   = 27745
  integer, public, parameter :: GH_COARSE = 83491

  !> @defgroup stencil_items Enumeration of stencil types.
  !> @{
  integer, public, parameter :: XORY1D     = 1
  integer, public, parameter :: X1D        = 2
  integer, public, parameter :: Y1D        = 3
  integer, public, parameter :: CROSS      = 4
  integer, public, parameter :: REGION     = 5
  integer, public, parameter :: CROSS2D    = 6

  !> @}

  !> Allows metadata types to be syntactically correct.
  !>
  !> This is a dummy array which the enumerators can index. It is not a real
  !> thing it is just there to ensure the compiler is happy.
  !>
  !> @todo In an ideal world this would be implemented as a function which
  !>       would remove the need for 1-based monotonically increasing
  !>       enumerator values but GFortran doesn't like that.
  !>
  integer, public, parameter :: STENCIL(6) = -1

  !> @defgroup mesh_data_items Enumeration of mesh data items.
  !> @{
  integer, public, parameter :: adjacent_face = 533
  !> @}

  !> @defgroup reference_element_data_items Enumeration of reference element data items.
  !> @{
  integer, public, parameter :: normals_to_faces                    = 171
  integer, public, parameter :: normals_to_horizontal_faces         = 904
  integer, public, parameter :: normals_to_vertical_faces           = 333
  integer, public, parameter :: outward_normals_to_faces            = 007
  integer, public, parameter :: outward_normals_to_horizontal_faces = 618
  integer, public, parameter :: outward_normals_to_vertical_faces   = 802
  !> @}

! Metadata argument type
  type, public :: arg_type
     integer :: arg_type         ! {GH_FIELD, GH_OPERATOR, GH_COLUMNWISE_OPERATOR, &
                                 !  GH_SCALAR}
     integer :: arg_intent       ! {GH_READ, GH_WRITE, GH_READWRITE, GH_INC, &
                                 !  GH_SUM, GH_MIN, GH_MAX}
     integer :: wspace      = -1 ! {W0, W1, W2, W3, ANY_SPACE_[0-9]+, ANY_W2}
     integer :: from_wspace = -1 ! { " } only required for gh_operator
     integer :: stencil_map = -1 !{XORY1D,X1D,Y1D,CROSS} optional, type of stencil map to use
     integer :: mesh_arg    = -1 !{GH_FINE,GH_COARSE} optional, for inter mesh mapping kernels
  end type arg_type

! Metadata argument function space type
  type, public :: func_type
     integer :: wspace            ! {W0, W1, W2, W3, ANY_SPACE_[0-9]+, ANY_W2}
     integer :: wproperties1      ! {GH_BASIS, GH_DIFF_BASIS, GH_OPERATOR, &
                                  !  GH_COLUMN_BANDED_DOFMAP, &
                                  !  GH_COLUMN_INDIRECTION_DOFMAP}
     integer :: wproperties2 = -1 ! { " } optional and must be a distinct property
     integer :: wproperties3 = -1 ! { " } optional and must be a distinct property
  end type func_type

  !> Metadata argument for mesh data requirements used in kernel metadata
  !>
  type, public :: mesh_data_type
    integer :: mesh_data_item ! {adjacent_face}
  end type mesh_data_type

  !> Metadata argument for reference element data requirements used in kernel metadata
  !>
  type, public :: reference_element_data_type
    ! {normals_to_faces, normals_to_horizontal_faces, normals_to_vertical_faces, &
    !  outward_normals_to_faces, outward_normals_to_horizontal_faces,            &
    !  outward_normals_to_vertical_faces}
    integer :: reference_element_data_item
  end type reference_element_data_type

end module argument_mod
