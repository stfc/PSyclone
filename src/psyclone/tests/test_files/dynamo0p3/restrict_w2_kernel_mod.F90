!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2023, Science and Technology Facilities Council.
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
!> @brief Perform the restriction operation of a fine W2 field to a coarse mesh
!> @details Restrict the W2 fine grid field over a number of cells into a
!!          W2 coarse grid field. The fine grid cells are must be exactly
!!          nested in a coarse grid cell. The coarse field is obtained by
!!          summing contributions from the fine field multiplied by weights.
!!          This method is only designed for the lowest order W2 spaces.
module restrict_w2_kernel_mod

use argument_mod,            only: arg_type,                  &
                                   GH_FIELD, GH_REAL,         &
                                   GH_READ, GH_WRITE,         &
                                   GH_COARSE, GH_FINE,        &
                                   ANY_SPACE_2, CELL_COLUMN
use constants_mod,           only: i_def, r_def, r_single, r_double
use fs_continuity_mod,       only: W2
use kernel_mod,              only: kernel_type
use reference_element_mod,   only: W, S, E, N, B

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the
!> PSy layer.
!>

type, public, extends(kernel_type) :: restrict_w2_kernel_type
  private
  type(arg_type) :: meta_args(2) = (/                                          &
       arg_type(GH_FIELD, GH_REAL, GH_WRITE, W2,          mesh_arg=GH_COARSE), &
       arg_type(GH_FIELD, GH_REAL, GH_READ,  ANY_SPACE_2, mesh_arg=GH_FINE  )  &
       /)
  integer :: operates_on = CELL_COLUMN
end type restrict_w2_kernel_type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------

public :: restrict_w2_code

  ! Generic interface for real32 and real64 types
  interface restrict_w2_code
    module procedure  &
      restrict_w2_code_r_single, &
      restrict_w2_code_r_double
  end interface

contains

  !> @brief Restrict a fine W2 field to a coarse mesh
  !> @param[in]     nlayers                  Number of layers in a model column
  !> @param[in]     cell_map                 A 2D index map of which fine grid
  !!                                         cells lie in the coarse grid cell
  !> @param[in]     ncell_fine_per_coarse_x  Number of fine cells per coarse
  !!                                         cell in the horizontal x-direction
  !> @param[in]     ncell_fine_per_coarse_x  Number of fine cells per coarse
  !!                                         cell in the horizontal y-direction
  !> @param[in]     ncell_fine               Number of cells in the partition
  !!                                         for the fine grid
  !> @param[in,out] coarse_field             Coarse grid W2 field to compute
  !> @param[in]     fine_field               Fine grid  W2 field to restrict
  !> @param[in]     undf_coarse              Total num of DoFs on the coarse
  !!                                         grid for this mesh partition
  !> @param[in]     map_coarse               DoFmap of cells on the coarse grid
  !> @param[in]     ndf                      Num of DoFs per cell on both grids
  !> @param[in]     undf_fine                Total num of DoFs on the fine grid
  !!                                         for this mesh partition
  !> @param[in]     map_fine                 DoFmap of cells on the fine grid

  ! R_SINGLE PRECISION
  ! ==================
  subroutine restrict_w2_code_r_single(nlayers,                 &
                                       cell_map,                &
                                       ncell_fine_per_coarse_x, &
                                       ncell_fine_per_coarse_y, &
                                       ncell_fine,              &
                                       coarse_field,            &
                                       fine_field,              &
                                       undf_coarse,             &
                                       map_coarse,              &
                                       ndf,                     &
                                       undf_fine,               &
                                       map_fine                 )

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_fine_per_coarse_x
    integer(kind=i_def), intent(in) :: ncell_fine_per_coarse_y
    integer(kind=i_def), intent(in) :: ncell_fine
    integer(kind=i_def), intent(in) :: ndf
    integer(kind=i_def), intent(in) :: undf_fine, undf_coarse

    ! Fields
    real(kind=r_single), intent(inout) :: coarse_field(undf_coarse)
    real(kind=r_single), intent(in)    :: fine_field(undf_fine)

    ! Maps
    integer(kind=i_def), intent(in) :: map_fine(ndf, ncell_fine)
    integer(kind=i_def), intent(in) :: map_coarse(ndf)
    integer(kind=i_def), intent(in) :: cell_map(ncell_fine_per_coarse_x, ncell_fine_per_coarse_y)

    ! Internal variables
    integer(kind=i_def) :: df, k, x_idx, y_idx, face
    real(kind=r_single) :: new_coarse(nlayers+1)

    integer(kind=i_def), parameter :: n_faces = 5
    integer(kind=i_def), parameter :: face_order(n_faces) = [W,S,E,N,B]
    integer(kind=i_def)            :: x_idx_start(n_faces)
    integer(kind=i_def)            :: x_idx_end(n_faces)
    integer(kind=i_def)            :: y_idx_start(n_faces)
    integer(kind=i_def)            :: y_idx_end(n_faces)

    !---------------------------------------------------------------------------
    ! Define cells to average over for each df
    !---------------------------------------------------------------------------

    ! The rows and columns forming the cell map match the arrangment
    ! of fine cells within the coarse cell

    ! These are aligned as follows with the LFRic directions:
    !         N
    !   |--------------|
    !   |    row 1     |
    !   |c            c|
    !   |o            o|
    ! W |l            l| E
    !   |              |
    !   |1           nx|
    !   |    row ny    |
    !   |--------------|
    !          S

    do face = 1, size(face_order)
      df = face_order(face)

      select case(df)
      case(N)
        ! N edge is first row of cell map
        x_idx_start(df) = 1
        x_idx_end(df)   = ncell_fine_per_coarse_x
        y_idx_start(df) = 1
        y_idx_end(df)   = 1

      case(S)
        ! S edge is last row of cell map
        x_idx_start(df) = 1
        x_idx_end(df)   = ncell_fine_per_coarse_x
        y_idx_start(df) = ncell_fine_per_coarse_y
        y_idx_end(df)   = ncell_fine_per_coarse_y

      case(W)
        ! W edge is first column of cell map
        x_idx_start(df) = 1
        x_idx_end(df)   = 1
        y_idx_start(df) = 1
        y_idx_end(df)   = ncell_fine_per_coarse_y

      case(E)
        ! E edge is last column of cell map
        x_idx_start(df) = ncell_fine_per_coarse_x
        x_idx_end(df)   = ncell_fine_per_coarse_x
        y_idx_start(df) = 1
        y_idx_end(df)   = ncell_fine_per_coarse_y

      case default
        x_idx_start(df) = 1
        x_idx_end(df)   = ncell_fine_per_coarse_x
        y_idx_start(df) = 1
        y_idx_end(df)   = ncell_fine_per_coarse_y

      end select
    end do

    !---------------------------------------------------------------------------
    ! Horizontal components
    !---------------------------------------------------------------------------

    do face = 1, 4
      df = face_order(face)
      new_coarse(:) = 0.0_r_single

      ! Build up 1D array of new coarse values for this column and face
      do y_idx = y_idx_start(df), y_idx_end(df)
        do x_idx = x_idx_start(df), x_idx_end(df)
          do k = 0, nlayers-1
            new_coarse(k+1) = new_coarse(k+1) + fine_field(map_fine(df,cell_map(x_idx,y_idx))+k)
          end do
        end do
      end do

      ! Copy over values into coarse field
      do k = 0, nlayers - 1
        coarse_field(map_coarse(df)+k) = new_coarse(k+1)
      end do
    end do

    !---------------------------------------------------------------------------
    ! Vertical components
    !---------------------------------------------------------------------------
    ! Only do bottom value of cell
    ! Loop over an extra layer to get the very top
    df = B
    new_coarse(:) = 0.0_r_single

    ! Build up 1D array of new coarse values for this column
    do y_idx = 1, ncell_fine_per_coarse_y
      do x_idx = 1, ncell_fine_per_coarse_x
        do k = 0, nlayers
          new_coarse(k+1) = new_coarse(k+1) + fine_field(map_fine(df,cell_map(x_idx,y_idx))+k)
        end do
      end do
    end do

    ! Copy over values into coarse field
    do k = 0, nlayers
      coarse_field(map_coarse(df)+k) = new_coarse(k+1)
    end do

  end subroutine restrict_w2_code_r_single

  ! R_DOUBLE PRECISION
  ! ==================
  subroutine restrict_w2_code_r_double(nlayers,                 &
                                       cell_map,                &
                                       ncell_fine_per_coarse_x, &
                                       ncell_fine_per_coarse_y, &
                                       ncell_fine,              &
                                       coarse_field,            &
                                       fine_field,              &
                                       undf_coarse,             &
                                       map_coarse,              &
                                       ndf,                     &
                                       undf_fine,               &
                                       map_fine                 )

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_fine_per_coarse_x
    integer(kind=i_def), intent(in) :: ncell_fine_per_coarse_y
    integer(kind=i_def), intent(in) :: ncell_fine
    integer(kind=i_def), intent(in) :: ndf
    integer(kind=i_def), intent(in) :: undf_fine, undf_coarse

    ! Fields
    real(kind=r_double), intent(inout) :: coarse_field(undf_coarse)
    real(kind=r_double), intent(in)    :: fine_field(undf_fine)

    ! Maps
    integer(kind=i_def), intent(in) :: map_fine(ndf, ncell_fine)
    integer(kind=i_def), intent(in) :: map_coarse(ndf)
    integer(kind=i_def), intent(in) :: cell_map(ncell_fine_per_coarse_x, ncell_fine_per_coarse_y)

    ! Internal variables
    integer(kind=i_def) :: df, k, x_idx, y_idx, face
    real(kind=r_double) :: new_coarse(nlayers+1)

    integer(kind=i_def), parameter :: n_faces = 5
    integer(kind=i_def), parameter :: face_order(n_faces) = [W,S,E,N,B]
    integer(kind=i_def)            :: x_idx_start(n_faces)
    integer(kind=i_def)            :: x_idx_end(n_faces)
    integer(kind=i_def)            :: y_idx_start(n_faces)
    integer(kind=i_def)            :: y_idx_end(n_faces)

    !---------------------------------------------------------------------------
    ! Define cells to average over for each df
    !---------------------------------------------------------------------------

    ! The rows and columns forming the cell map match the arrangment
    ! of fine cells within the coarse cell

    ! These are aligned as follows with the LFRic directions:
    !         N
    !   |--------------|
    !   |    row 1     |
    !   |c            c|
    !   |o            o|
    ! W |l            l| E
    !   |              |
    !   |1           nx|
    !   |    row ny    |
    !   |--------------|
    !          S

    do face = 1, size(face_order)
      df = face_order(face)

      select case(df)
      case(N)
        ! N edge is first row of cell map
        x_idx_start(df) = 1
        x_idx_end(df)   = ncell_fine_per_coarse_x
        y_idx_start(df) = 1
        y_idx_end(df)   = 1

      case(S)
        ! S edge is last row of cell map
        x_idx_start(df) = 1
        x_idx_end(df)   = ncell_fine_per_coarse_x
        y_idx_start(df) = ncell_fine_per_coarse_y
        y_idx_end(df)   = ncell_fine_per_coarse_y

      case(W)
        ! W edge is first column of cell map
        x_idx_start(df) = 1
        x_idx_end(df)   = 1
        y_idx_start(df) = 1
        y_idx_end(df)   = ncell_fine_per_coarse_y

      case(E)
        ! E edge is last column of cell map
        x_idx_start(df) = ncell_fine_per_coarse_x
        x_idx_end(df)   = ncell_fine_per_coarse_x
        y_idx_start(df) = 1
        y_idx_end(df)   = ncell_fine_per_coarse_y

      case default
        x_idx_start(df) = 1
        x_idx_end(df)   = ncell_fine_per_coarse_x
        y_idx_start(df) = 1
        y_idx_end(df)   = ncell_fine_per_coarse_y

      end select
    end do

    !---------------------------------------------------------------------------
    ! Horizontal components
    !---------------------------------------------------------------------------

    do face = 1, 4
      df = face_order(face)
      new_coarse(:) = 0.0_r_double

      ! Build up 1D array of new coarse values for this column and face
      do y_idx = y_idx_start(df), y_idx_end(df)
        do x_idx = x_idx_start(df), x_idx_end(df)
          do k = 0, nlayers-1
            new_coarse(k+1) = new_coarse(k+1) + fine_field(map_fine(df,cell_map(x_idx,y_idx))+k)
          end do
        end do
      end do

      ! Copy over values into coarse field
      do k = 0, nlayers - 1
        coarse_field(map_coarse(df)+k) = new_coarse(k+1)
      end do
    end do

    !---------------------------------------------------------------------------
    ! Vertical components
    !---------------------------------------------------------------------------

    ! Only do bottom value of cell
    ! Loop over an extra layer to get the very top
    df = B
    new_coarse(:) = 0.0_r_double

    ! Build up 1D array of new coarse values for this column
    do y_idx = 1, ncell_fine_per_coarse_y
      do x_idx = 1, ncell_fine_per_coarse_x
        do k = 0, nlayers
          new_coarse(k+1) = new_coarse(k+1) + fine_field(map_fine(df,cell_map(x_idx,y_idx))+k)
        end do
      end do
    end do

    ! Copy over values into coarse field
    do k = 0, nlayers
      coarse_field(map_coarse(df)+k) = new_coarse(k+1)
    end do

  end subroutine restrict_w2_code_r_double

end module restrict_w2_kernel_mod
