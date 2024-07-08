! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
! Author: A. R. Porter, STFC Daresbury Lab

! Example of a Kernel containing a call to another routine.
module testkern_with_call_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_with_call_type
  private
  type(arg_type) :: meta_args(4) = (/                         &
       arg_type(gh_field, gh_real, gh_write, w3),             &
       arg_type(gh_field * 3, gh_real, gh_read, any_space_9), &
       arg_type(gh_scalar, gh_real, gh_read),                 &
       arg_type(gh_scalar, gh_real, gh_read)/)
  TYPE(func_type) :: meta_funcs(1) = (/func_type(any_space_9, gh_basis)/)
  INTEGER :: operates_on = cell_column
  INTEGER :: gh_shape = gh_evaluator
   contains
     procedure, nopass :: code => testkern_with_call_code
  end type testkern_with_call_type

contains

  subroutine testkern_with_call_code(nlayers, phi, chi_1, chi_2, chi_3, &
       gravity, planet_radius, ndf_w3, undf_w3, map_w3, &
       ndf_chi, undf_chi, map_chi, chi_basis)
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in) :: ndf_chi
    integer(kind=i_def), intent(in) :: undf_chi
    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
    integer(kind=i_def), dimension(ndf_chi), intent(in) :: map_chi
    real(kind=r_def), dimension(undf_w3), intent(inout) :: phi
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi_1
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi_2
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi_3
    real(kind=r_def), dimension(1,ndf_chi,ndf_w3), intent(in) :: chi_basis
    real(kind=r_def), intent(in) :: gravity
    real(kind=r_def), intent(in) :: planet_radius
    integer(kind=i_def) :: df
    integer(kind=i_def) :: dfc
    integer(kind=i_def) :: k
    real(kind=r_def), dimension(3) :: coord
    real(kind=r_def) :: lat
    real(kind=r_def) :: lon
    real(kind=r_def) :: radius
    real(kind=r_def) :: shallow_switch
    real(kind=r_def) :: height
    real(kind=r_def), dimension(ndf_chi) :: chi_1_e
    real(kind=r_def), dimension(ndf_chi) :: chi_2_e
    real(kind=r_def), dimension(ndf_chi) :: chi_3_e
    real(kind=r_def) :: phi_shallow
    real(kind=r_def) :: phi_deep

    do k = 0, nlayers-1
       do dfc = 1, ndf_chi
          chi_1_e(dfc) = chi_1( map_chi(dfc) + k)
          chi_2_e(dfc) = chi_2( map_chi(dfc) + k)
          chi_3_e(dfc) = chi_3( map_chi(dfc) + k)
       end do

       do df = 1, ndf_w3
          coord(:) = 0.0_r_def
          do dfc = 1, ndf_chi
             coord(1) = coord(1) + chi_1_e(dfc)*chi_basis(1,dfc,df)
             coord(2) = coord(2) + chi_2_e(dfc)*chi_basis(1,dfc,df)
             coord(3) = coord(3) + chi_3_e(dfc)*chi_basis(1,dfc,df)
          end do
          call xyz2llr(coord(1), coord(2), coord(3), lon, lat, radius)
          phi_shallow = gravity * radius
          phi_deep = -gravity * planet_radius * &
               (planet_radius / radius - 1.0_r_def)
          phi(map_w3(df) + k) = shallow_switch * phi_shallow + &
               (1.0_r_def - shallow_switch) * phi_deep
       end do
    end do

  end subroutine testkern_with_call_code

end module testkern_with_call_mod
