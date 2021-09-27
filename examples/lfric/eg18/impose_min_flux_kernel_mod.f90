!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
! ----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2021, Science and Technology Facilities Council
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
! Modified: R. W. Ford, STFC Daresbury Lab

module impose_min_flux_kernel_mod

use argument_mod,            only : arg_type,                         &
                                    GH_FIELD, GH_OPERATOR, GH_SCALAR, &
                                    GH_REAL, GH_READ, GH_READINC,     &
                                    CELL_COLUMN
                                    
use fs_continuity_mod,       only : W3, W2
use constants_mod,           only : r_def, i_def, tiny_eps, l_def, EPS
use kernel_mod,              only : kernel_type
use log_mod,                 only : log_event,           &
                                    log_scratch_space,   &
                                    LOG_LEVEL_INFO

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: impose_min_flux_kernel_type
  private
  type(arg_type) :: meta_args(6) = (/                      &
       arg_type(GH_FIELD,    GH_REAL, GH_READ,    W3),     &
       arg_type(GH_FIELD,    GH_REAL, GH_READINC, W2),     &
       arg_type(GH_OPERATOR, GH_REAL, GH_READ,    W3, W2), &
       arg_type(GH_OPERATOR, GH_REAL, GH_READ,    W3, W3), &
       arg_type(GH_SCALAR,   GH_REAL, GH_READ),            &
       arg_type(GH_SCALAR,   GH_REAL, GH_READ)             &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: impose_min_flux_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: impose_min_flux_code

contains

!> @brief Modify the flux so the updated field f(n+1) is greater than field_min
!!        i.e., f(n+1) = f(n) - dts*div(flux) >= field_min
!! @param[in] cell Horizontal cell index
!! @param[in] nlayers Number of layers
!! @param[in] field Field at level time n
!! @param[in,out] flux Input/output
!! @param[in] ncell_3d1 Total number of cells related to div
!! @param[in] div Divergence operator used in the update
!! @param[in] ncell_3d2 Total number of cells related to div_multiplier
!! @param[in] div_multiplier Divergence multiplier used in the update
!! @param[in] field_min The minimum value we want to enfore for the updated field
!! @param[in] dts  The time-step used in the update
!! @param[in] ndf1 Number of degrees of freedom per cell for the output field
!! @param[in] undf1 Unique number of degrees of freedom  for the output field
!! @param[in] map1 Dofmap for the cell at the base of the column for the output field
!! @param[in] ndf2 Number of degrees of freedom per cell for the input field
!! @param[in] undf2 Unique number of degrees of freedom for the input field
!! @param[in] map2 Dofmap for the cell at the base of the column for the input field
subroutine impose_min_flux_code(cell,              &
                                nlayers,           &
                                field, flux,       &
                                ncell_3d1,         &
                                div,               &
                                ncell_3d2,         &
                                div_multiplier,    &
                                field_min,         &
                                dts,               &
                                ndf1, undf1, map1, &
                                ndf2, undf2, map2  )

  implicit none

  ! Arguments
  integer(kind=i_def),                  intent(in) :: cell, nlayers
  integer(kind=i_def),                  intent(in) :: ncell_3d1, ncell_3d2
  integer(kind=i_def),                  intent(in) :: undf1, ndf1
  integer(kind=i_def),                  intent(in) :: undf2, ndf2
  integer(kind=i_def), dimension(ndf1), intent(in) :: map1
  integer(kind=i_def), dimension(ndf2), intent(in) :: map2
  real(kind=r_def), dimension(undf2),               intent(inout) :: flux
  real(kind=r_def), dimension(undf1),               intent(in)    :: field
  real(kind=r_def), dimension(ndf1,ndf2,ncell_3d1), intent(in)    :: div
  real(kind=r_def), dimension(ndf1,ndf1,ncell_3d2), intent(in)    :: div_multiplier
  real(kind=r_def), intent(in)                                    :: field_min
  real(kind=r_def), intent(in)                                    :: dts

  ! Internal variables
  integer(kind=i_def)                  :: k, ik, df1, df2
  real(kind=r_def), dimension(ndf2)    :: cell_fluxes
  real(kind=r_def)                     :: a, b, inc, inc_n, flux_scaler
  integer(kind=i_def), dimension(ndf2) :: flux_change_id

  do k = 0, nlayers-1

    ! flux is read (gathered) here so needs to have valid values,
    ! including any halo values that are accessed.
    do df2 = 1, ndf2
      cell_fluxes(df2) = flux(map2(df2)+k)
    end do
    ik = (cell-1)*nlayers + k + 1

    do df1 = 1, ndf1

       inc_n = 0.0_r_def
       flux_change_id = 0_i_def

       do df2 = 1, ndf2
         inc = - dts*div(df1,df2,ik)*div_multiplier(df1,df1,ik)*cell_fluxes(df2)
         if ( inc < 0.0_r_def ) then
             inc_n = inc_n - inc
             flux_change_id(df2) = 1_i_def
         end if
       end do

       a = field(map1(df1)+k) - (field_min + EPS)
       b = a / max(inc_n, EPS)
       flux_scaler = min(max(0.0_r_def,b),1.0_r_def)

       ! flux is incremented here and is continuous in the horizontal
       ! so for this access the outermost halo region that is accessed
       ! does not need to contain valid flux values (as the resultant
       ! flux values will be incorrect even if the flux values were
       ! valid). However the kernel itself does require these flux
       ! values to be valid due to the earlier read, which makes the
       ! kernel flux access GH_READINC.
       do df2 = 1, ndf2
          if ( flux_change_id(df2) == 1_i_def ) then
              flux(map2(df2)+k) = flux(map2(df2)+k) * flux_scaler
          end if
       end do

    end do

  end do

end subroutine impose_min_flux_code

end module impose_min_flux_kernel_mod
