! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! A kernel that initialises a perturbation field on W3 function space to:
!   perturbation = ampl(z)*exp( -((x - x_centre)/half_width_x)**2 &
!                               -((y - y_centre)/half_width_y)**2 )
!   where ampl(z) = max(perturbation_height - z, 0)/perturbation_scale
module init_perturbation_kernel_mod

  use argument_mod,      only: arg_type, func_type,   &
                               GH_FIELD, GH_REAL,     &
                               GH_READ, GH_READWRITE, &
                               CELL_COLUMN
  use fs_continuity_mod, only: W3
  use constants_mod,     only: r_def, i_def
  use kernel_mod,        only: kernel_type
  use perturbation_bell_config_mod, &
                         only: half_width_x, half_width_y, &
                               perturbation_scale,         &
                               perturbation_height,        &
                               x_centre, y_centre

  implicit none

  private

  !-----------------------------------------------------------------------------
  ! The type declaration for the kernel. Contains the metadata needed by
  ! the PSy layer.
  !-----------------------------------------------------------------------------
  type, public, extends(kernel_type) :: init_perturbation_kernel_type
    private
    type(arg_type), dimension(2) :: meta_args = (/        &
         arg_type(GH_FIELD,   GH_REAL, GH_READWRITE, W3), &
         arg_type(GH_FIELD*3, GH_REAL, GH_READ,      W3)  &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: init_perturbation_code
  end type init_perturbation_kernel_type

  public init_perturbation_code

  contains

  !> @brief Initialise a perturbation field using coordinate and namelist data
  !> @param[in] nlayers Number of layers
  !> @param[in,out] perturbation Perturbation field to initialise
  !> @param[in] chi_1 Coordinates in the x direction
  !> @param[in] chi_2 Coordinates in the y direction
  !> @param[in] chi_3 Coordinates in the z direction
  !> @param[in] ndf_w3 Number of degrees of freedom per cell for the
  !!                   perturbation field
  !> @param[in] undf_w3 Number of unique degrees of freedom for the
  !!                   perturbation field
  !> @param[in] map_w3 Dofmap for the cell at the base of the column for the
  !!                   perturbation field
  subroutine init_perturbation_code(nlayers, perturbation, &
                                    chi_1, chi_2, chi_3,   &
                                    ndf_w3, undf_w3, map_w3)

    implicit none

    ! Arguments
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    real(kind=r_def), intent(inout), dimension(undf_w3) :: perturbation
    real(kind=r_def), intent(in),    dimension(undf_w3) :: chi_1
    real(kind=r_def), intent(in),    dimension(undf_w3) :: chi_2
    real(kind=r_def), intent(in),    dimension(undf_w3) :: chi_3

    ! Internal variables
    integer(kind=i_def)  :: k, df
    real(kind=r_def)     :: x(3), xt, yt, ampl

    ! Initialise perturbation field
    do k = 0, nlayers-1

      do df = 1, ndf_w3

        ! Get coordinate values on each DoF
        x(1) = chi_1(map_w3(df) + k)
        x(2) = chi_2(map_w3(df) + k)
        x(3) = chi_3(map_w3(df) + k)

        !-----------------------------------------------------------------------
        ! TO COMPLETE: Initialise perturbation field to the prescribed
        ! analytical expression on each DoF, i.e. perturbation( map_w3(df) + k )
        !-----------------------------------------------------------------------

      end do

    end do

  end subroutine init_perturbation_code

end module init_perturbation_kernel_mod
