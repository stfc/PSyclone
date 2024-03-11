

!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
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
!          A. R. Porter and R. W. Ford, STFC Daresbury Laboratory


!> @brief Metadata for the kernel arguments required by the PSy layer.

!> @details In order to create the correct PSy code, PSyclone requires several
!!          kernel argument properties. These properties are stored in kernels
!!          as the kernel metadata descriptors (see PSyclone documentation:
!!          https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#metadata).
!!          The elements of PSyclone LFRic API kernel metadata are:
!!
!!          1) `type(arg_type) :: meta_args(...)` that describes properties of
!!             kernel arguments (e.g.\ argument type, access);
!!
!!          2) `type(func_type) :: meta_funcs(...)` that describes the required
!!             basis/differential basis functions information;
!!
!!          3) `type(reference_element_data_type) :: meta_reference_element(...`
!!             that describes the required reference element properties
!!             information;
!!
!!          4) `type(mesh_data_type) :: meta_mesh(...)` that describes the
!!             required mesh properties information;
!!
!!          5) `gh_shape = ...` (e.g.\ `gh_shape = gh_quadrature_XYoZ` that
!!             describes the required quadrature and/or evaluator properties
!!             information;
!!
!!          6) `operates_on` metadata that describes what the kernel updates,
!!             e.g.\ a vertical single-cell column;
!!
!!          7) `procedure` metadata that specifies the name of the kernel
!!             subroutine that the metadata describes.
!!
!!          `type(arg_type) :: meta_args(...)`, `operates_on` and
!!          the `procedure` metadata are mandatory for all kernels.
module argument_mod

  implicit none

  private

  !> @defgroup argument_type Enumeration of argument type property descriptors.
  !> @{
  integer, public, parameter :: GH_SCALAR              = 397
  integer, public, parameter :: GH_FIELD               = 507
  integer, public, parameter :: GH_OPERATOR            = 735
  integer, public, parameter :: GH_COLUMNWISE_OPERATOR = 841
  !> @}

  !> @defgroup data_type Enumeration of argument data type property descriptors.
  !> @{
  integer, public, parameter :: GH_REAL    = 58
  integer, public, parameter :: GH_INTEGER = 5
  integer, public, parameter :: GH_LOGICAL = 987
  !> @}

  !> @defgroup argument_access Enumeration of argument access property descriptors.
  !> @{
  integer, public, parameter :: GH_READ      = 958
  integer, public, parameter :: GH_WRITE     = 65
  integer, public, parameter :: GH_READWRITE = 811
  integer, public, parameter :: GH_INC       = 542
  integer, public, parameter :: GH_READINC   = 420
  integer, public, parameter :: GH_SUM       = 563
  integer, public, parameter :: GH_MIN       = 718
  integer, public, parameter :: GH_MAX       = 391
  !> @}

  !> @defgroup fspace Enumeration of function space IDs (also "fspace_from").
  !> @details This module defines three types of generalised function space IDs:
  !!          1) "ANY_SPACE_[1-10]+" for generalised function spaces regardless
  !!              of their continuity;
  !!          2) "ANY_W2" for any `W2`-type space regardless of its continuity;
  !!          3) "ANY_DISCONTINUOUS_SPACE_[1-10]+" for generalised discontinuous
  !!             function spaces.
  !!          Distinct IDs are required as we may have groups of fields that
  !!          must be on the same space within a kernel.
  !! @{
  ! IDs for any space regardless of continuity.
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
  ! IDs for any W2-type space regardless of continuity
  ! (w2, w2h, w2v, w2broken but not w2*trace spaces of scalar
  ! functions). Issue #540 will resolve what W2* spaces should
  ! be included in ANY_W2 list and how they should be treated.
  integer, public, parameter :: ANY_W2         = 353
  ! IDs for any discontinuous space.
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
  !> @}

  !> @defgroup stencil_map Enumeration of stencil access map types.
  !> @{
  integer, public, parameter :: XORY1D  = 1
  integer, public, parameter :: X1D     = 2
  integer, public, parameter :: Y1D     = 3
  integer, public, parameter :: CROSS   = 4
  integer, public, parameter :: REGION  = 5
  integer, public, parameter :: CROSS2D = 6
  !> @}

  !> Allows metadata types to be syntactically correct.
  !>
  !> This is a dummy array which the enumerators can index. It is not a real
  !! thing (it is just there to ensure the compiler is happy).
  !>
  !> @todo In an ideal world this would be implemented as a function which
  !!       would remove the need for 1-based enumerator values but
  !!       GFortran doesn't like that.
  integer, public, parameter :: STENCIL(6) = -1

  !> @defgroup mesh_arg Enumeration of coarse and fine function spaces
  !>                    for inter-grid mapping kernels.
  !> @{
  integer, public, parameter :: GH_FINE   = 27745
  integer, public, parameter :: GH_COARSE = 83491
  !> @}

  !> @defgroup fspace_basis Enumeration of function space basis/differential
  !!                        basis property descriptors (also "fspace_properties1").
  !> @{
  integer, public, parameter :: GH_BASIS      = 751
  integer, public, parameter :: GH_DIFF_BASIS = 767
  !> @}

  !> @defgroup gh_shape Enumeration of quadrature and/or evaluator property
  !!                    descriptors (if a kernel requires basis or
  !!                    differential-basis functions).
  !> @{
  ! Quadrature metadata
  integer, public, parameter :: GH_QUADRATURE_XYZ   = 912
  integer, public, parameter :: GH_QUADRATURE_XYoZ  = 849
  integer, public, parameter :: GH_QUADRATURE_XoYoZ = 701
  integer, public, parameter :: GH_QUADRATURE_face  = 539
  integer, public, parameter :: GH_QUADRATURE_edge  = 419
  ! Evaluator metadata
  integer, public, parameter :: GH_EVALUATOR        = 959
  !> @}

  !> @defgroup reference_element_data_items Enumeration of reference element
  !!                                        data items.
  !> @{
  integer, public, parameter :: normals_to_faces                    = 171
  integer, public, parameter :: normals_to_horizontal_faces         = 904
  integer, public, parameter :: normals_to_vertical_faces           = 333
  integer, public, parameter :: outward_normals_to_faces            = 007
  integer, public, parameter :: outward_normals_to_horizontal_faces = 618
  integer, public, parameter :: outward_normals_to_vertical_faces   = 802
  !> @}

  !> @defgroup mesh_data_items Enumeration of mesh data items.
  !> @{
  integer, public, parameter :: adjacent_face = 533
  !> @}

  !> @defgroup operates_on Enumeration of kernel iterator property descriptors.
  !> @{
  integer, public, parameter :: CELL_COLUMN = 396
  integer, public, parameter :: DOMAIN      = 945
  integer, public, parameter :: DOF         = 712
  !> @}

  !> Metadata for the argument type description, stored in the `arg_type` type
  !! as an array of `meta_args`. We need to know how many scalars, fields
  !! and/or operators are passed to the kernel and in what order they are
  !! passed. We also need to know how these scalars/fields/operators:
  !! - Are accessed (read, write, etc.) within the kernel;
  !! - What is the type of argument data;
  !! - What function space the fields and operators are on (w0, w1, etc.).
  !! In the case of operators there are two function spaces (to and from).
  !! Fields may have an optional metadata describing either a stencil access
  !! or, for inter-grid kernels, which mesh the field is on.
  type, public :: arg_type
     !> Type of a kernel argument (scalar, field, operator or a
     !! column-wise operator). One of {GH_SCALAR, GH_FIELD, GH_OPERATOR,
     !! GH_COLUMNWISE_OPERATOR}.
     integer :: argument_type
     !> Fortran primitive type of kernel argument data.
     !! One of {GH_REAL, GH_INTEGER}.
     integer :: data_type
     !> How the kernel argument data is accessed (e.g.\ read-only, update,
     !! global reduction). One of {GH_READ, GH_WRITE, GH_READWRITE,
     !! GH_INC, GH_SUM, GH_MIN, GH_MAX}.
     integer :: argument_access
     !> Function space "on" of a field argument or "to" of an operator argument.
     !! One of {W*, ANY_SPACE_[1-10]+, ANY_DISCONTINUOUS_SPACE_[1-10]+, ANY_W2}.
     integer :: fspace      = -1
     !> Function space "from" of an operator argument. One of {W*,
     !! ANY_SPACE_[1-10]+, ANY_DISCONTINUOUS_SPACE_[1-10]+, ANY_W2}.
     integer :: fspace_from = -1
     !> Optional metadata (fields only) for a type of stencil map to use.
     !! One of {XORY1D, X1D, Y1D, CROSS, REGION, CROSS2D}.
     integer :: stencil_map = -1
     !> Optional metadata (fields only) for inter-grid mapping kernels.
     !! One of {GH_FINE, GH_COARSE}.
     integer :: mesh_arg    = -1
  end type arg_type

  !> Optional metadata for the basis/differential basis functions required with
  !! the quadrature or evaluator data a particular function space, stored in
  !! the `func_type` type as an array of `meta_funcs`). This information is
  !! required when specifying `gh_shape` information for Gaussian quadrature
  !! type or evaluator target).
  type, public :: func_type
     !> Function space "on" of basis/differential basis functions. One of
     !! {W*, ANY_SPACE_[1-10]+, ANY_DISCONTINUOUS_SPACE_[1-10]+, ANY_W2}.
     integer :: fspace
     !> Basis/differential basis functions on a specified function space.
     !! One of {GH_BASIS, GH_DIFF_BASIS}.
     integer :: fspace_basis
     !> Optional metadata ({ "" }), must be a distinct property
     !! (e.g.\ differential basis functions if basis functions are already
     !! specified). One of {GH_BASIS, GH_DIFF_BASIS}.
     integer :: fspace_properties1 = -1
  end type func_type

  !> Optional metadata for the reference element data, stored in the
  !! `reference_element_data_type` type as an array of `meta_reference_element`.
  type, public :: reference_element_data_type
    !> One of {normals_to_faces, normals_to_horizontal_faces,
    !! normals_to_vertical_faces, outward_normals_to_faces,
    !! outward_normals_to_horizontal_faces, outward_normals_to_vertical_faces}.
    integer :: reference_element_data_item
  end type reference_element_data_type

  !> Optional metadata for the mesh data, stored in the `mesh_data_type` type as
  !! an array of `meta_mesh` (currently only supports `adjacent_face` array).
  type, public :: mesh_data_type
    integer :: mesh_data_item
  end type mesh_data_type

end module argument_mod
