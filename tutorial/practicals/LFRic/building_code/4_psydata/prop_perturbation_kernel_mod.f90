! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
! Author: I. Kavcic, Met Office
!
! -----------------------------------------------------------------------------
! A kernel that propagates a perturbation field on W3 function space as:
!   perturbation = max(perturbation_height - z, 0)/perturbation_scale* &
!                      exp( -((x - x_centre - u_vel*t_tot)/half_width_x)**2 &
!                           -((y - y_centre - v_vel*t_tot)/half_width_y)**2 )
! -----------------------------------------------------------------------------
module prop_perturbation_kernel_mod

  use argument_mod,      only: arg_type, func_type,   &
                               GH_FIELD, GH_REAL,     &
                               GH_SCALAR,             &
                               GH_READWRITE, GH_READ, &
                               CELL_COLUMN
  use fs_continuity_mod, only: W3
  use constants_mod,     only: r_def, i_def
  use kernel_mod,        only: kernel_type
  use perturbation_bell_config_mod, &
                         only: half_width_x, half_width_y, &
                               perturbation_scale,         &
                               perturbation_height,        &
                               x_centre, y_centre, u_vel, v_vel

  implicit none

  private

  !-----------------------------------------------------------------------------
  ! The type declaration for the kernel. Contains the metadata needed by
  ! the PSy layer.
  !-----------------------------------------------------------------------------
  type, public, extends(kernel_type) :: prop_perturbation_kernel_type
    private
    type(arg_type), dimension(3) :: meta_args = (/        &
         arg_type(GH_FIELD,   GH_REAL, GH_READWRITE, W3), &
         arg_type(GH_FIELD*3, GH_REAL, GH_READ,      W3), &
         arg_type(GH_SCALAR,  GH_REAL, GH_READ)           &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: prop_perturbation_code
  end type prop_perturbation_kernel_type

  public prop_perturbation_code

  contains

  !> @brief Initialise a perturbation field using coordinate and namelist data
  !> @param[in] nlayers Number of layers
  !> @param[in,out] perturbation Perturbation field to propagate
  !> @param[in] chi_1 Coordinates in the x direction
  !> @param[in] chi_2 Coordinates in the y direction
  !> @param[in] chi_3 Coordinates in the z direction
  !> @param[in] t_tot Time (s) to propagate the perturbation field for
  !> @param[in] ndf_w3 Number of degrees of freedom per cell for the
  !!                   perturbation field
  !> @param[in] undf_w3 Number of unique degrees of freedom for the
  !!                   perturbation field
  !> @param[in] map_w3 Dofmap for the cell at the base of the column for the
  !!                   perturbation field
  subroutine prop_perturbation_code(nlayers, perturbation,      &
                                    chi_1, chi_2, chi_3, t_tot, &
                                    ndf_w3, undf_w3, map_w3)

    use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
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
    real(kind=r_def), intent(in) :: t_tot

    integer(kind=8) :: offset

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
        ! TO COMPLETE: Propagate perturbation field as in the prescribed
        ! analytical expression on each DoF, i.e. perturbation( map_w3(df) + k )
        ampl = max(perturbation_height - x(3), 0.0_r_def)/perturbation_scale
        xt = ( x(1) - x_centre - u_vel*t_tot )/half_width_x
        yt = ( x(2) - y_centre - v_vel*t_tot )/half_width_y
        perturbation( map_w3(df) + k ) = ampl*exp(-xt**2 - yt**2)
        !-----------------------------------------------------------------------

      end do

    end do

    ! ---------------------------------------------------------------------------------------------
    ! FOR READONLY VERIFICATION:
    ! ---------------------------------------------------------------------------------------------
    ! VERY DIRTY CODE HERE. REMOVE THE COMMENT SYMBOLS BELOW TO OVERWRITE
    ! ONE ENTRY OF chi_1. YOU MUST MAKE SURE TO HAVE ARRAY BOUNDS CHECKING
    ! DISABLED IN THE COMPILER
    !offset = ( loc(chi_1(map_w3(1)))- loc(perturbation(map_w3(1)))) / sizeof(chi_1(map_w3(1)))
    !perturbation(map_w3(1)+offset) = 123.0
    ! ---------------------------------------------------------------------------------------------


    ! ---------------------------------------------------------------------------------------------
    ! FOR NAN VERIFICATION:
    ! ---------------------------------------------------------------------------------------------
    ! Set a NAN into one return value:
    !perturbation( map_w3(1) ) = IEEE_VALUE(perturbation( map_w3(1) ), IEEE_QUIET_NAN)
    ! ---------------------------------------------------------------------------------------------

  end subroutine prop_perturbation_code

end module prop_perturbation_kernel_mod
