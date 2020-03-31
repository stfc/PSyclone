!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-----------------------------------------------------------------------------
!
! BSD 3-Clause License
!
! Modifications copyright (c) 2017-2020, Science and Technology Facilities
! Council.
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Modifications: A. R. Porter, STFC Daresbury Lab,
!                I. Kavcic, Met Office

module reference_element_mod

  use constants_mod, only: r_def, i_def

  implicit none

  private

  type, abstract, public :: reference_element_type
  contains

    procedure, public :: get_number_horizontal_faces
    procedure, public :: get_number_vertical_faces
    procedure, public :: get_number_faces
    procedure, public :: get_normals_to_horizontal_faces
    procedure, public :: get_outward_normals_to_horizontal_faces
    procedure, public :: get_normals_to_vertical_faces
    procedure, public :: get_outward_normals_to_vertical_faces
    procedure, public :: get_normals_to_faces
    procedure, public :: get_outward_normals_to_faces

  end type reference_element_type

contains

  ! Gets the number of horizontal faces in the reference element
  integer function get_number_horizontal_faces(this)
    implicit none
    class(reference_element_type), intent(in)  :: this
    get_number_horizontal_faces = 0
  end function get_number_horizontal_faces

  ! Gets the number of vertical faces in the reference element
  integer function get_number_vertical_faces(this)
    implicit none
    class(reference_element_type), intent(in)  :: this
    get_number_vertical_faces = 0
  end function get_number_vertical_faces

  ! Gets the number of all faces in the reference element
  integer function get_number_faces(this)
    implicit none
    class(reference_element_type), intent(in)  :: this
    get_number_faces = 0
  end function get_number_faces

  ! Gets the array of vectors normal to horizontal faces
  subroutine get_normals_to_horizontal_faces(this, normals)
    implicit none
    class(reference_element_type), intent(in)  :: this
    real(r_def), allocatable,      intent(out) :: normals(:,:)
    allocate( normals(1,1) )
  end subroutine get_normals_to_horizontal_faces

  ! Gets the array of vectors normal to "outward" horizontal faces
  subroutine get_outward_normals_to_horizontal_faces(this, normals)
    implicit none
    class(reference_element_type), intent(in)  :: this
    real(r_def), allocatable,      intent(out) :: normals(:,:)
    allocate( normals(1,1) )
  end subroutine get_outward_normals_to_horizontal_faces

  ! Gets the array of vectors normal to vertical faces
  subroutine get_normals_to_vertical_faces(this, normals)
    implicit none
    class(reference_element_type), intent(in)  :: this
    real(r_def), allocatable,      intent(out) :: normals(:,:)
    allocate( normals(1,1) )
  end subroutine get_normals_to_vertical_faces

  ! Gets the array of vectors normal to "outward" vertical faces
  subroutine get_outward_normals_to_vertical_faces(this, normals)
    implicit none
    class(reference_element_type), intent(in)  :: this
    real(r_def), allocatable,      intent(out) :: normals(:,:)
    allocate( normals(1,1) )
  end subroutine get_outward_normals_to_vertical_faces

  ! Gets the array of vectors normal to all faces
  subroutine get_normals_to_faces(this, normals)
    implicit none
    class(reference_element_type), intent(in)  :: this
    real(r_def), allocatable,      intent(out) :: normals(:,:)
    allocate( normals(1,1) )
  end subroutine get_normals_to_faces

  ! Gets the array of vectors normal to all "outward" faces
  subroutine get_outward_normals_to_faces(this, normals)
    implicit none
    class(reference_element_type), intent(in)  :: this
    real(r_def), allocatable,      intent(out) :: normals(:,:)
    allocate( normals(1,1) )
  end subroutine get_outward_normals_to_faces

end module reference_element_mod
