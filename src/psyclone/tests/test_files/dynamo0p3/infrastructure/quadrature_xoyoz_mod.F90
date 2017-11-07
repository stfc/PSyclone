!-----------------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! For further details please refer to the file LICENCE which you should have
! received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------
!
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
! Modifications: A. R. Porter, STFC Daresbury Lab

module quadrature_xoyoz_mod

use constants_mod,           only: r_def, i_def, PI, EPS
use quadrature_rule_mod,     only: quadrature_rule_type
use abstract_quadrature_mod, only: abstract_quadrature_type
use function_space_mod,      only: function_space_type

implicit none

private

type, public, extends(abstract_quadrature_type) :: quadrature_xoyoz_type

  private

  real(kind=r_def), allocatable :: weights_x(:), weights_y(:), weights_z(:)
  real(kind=r_def), allocatable :: points_x(:), points_y(:), points_z(:)
  integer(kind=i_def) :: np_x, np_y, np_z

contains

  procedure, public :: get_quadrature_proxy
  procedure, public :: compute_function

end type quadrature_xoyoz_type

type, public :: quadrature_xoyoz_proxy_type

  private
  real(kind=r_def), pointer, public :: weights_x(:) => null()
  real(kind=r_def), pointer, public :: weights_y(:) => null()
  real(kind=r_def), pointer, public :: weights_z(:) => null()
  real(kind=r_def), pointer, public :: points_x(:)  => null()
  real(kind=r_def), pointer, public :: points_y(:)  => null()
  real(kind=r_def), pointer, public :: points_z(:)  => null()

  integer(kind=i_def), public       :: np_x, np_y, np_z

contains

end type quadrature_xoyoz_proxy_type

contains


type(quadrature_xoyoz_proxy_type) function get_quadrature_proxy(self)

  implicit none

  class(quadrature_xoyoz_type), target, intent(in)  :: self

  get_quadrature_proxy % points_x  => null()
  get_quadrature_proxy % points_y  => null()
  get_quadrature_proxy % points_z  => null()
  get_quadrature_proxy % weights_x => null()
  get_quadrature_proxy % weights_y => null()
  get_quadrature_proxy % weights_z => null()
  get_quadrature_proxy % np_x      = 0
  get_quadrature_proxy % np_y      = 0
  get_quadrature_proxy % np_z      = 0

end function get_quadrature_proxy

subroutine compute_function(self, function_to_call, function_space, &
                            fspace_dim, ndf, basis)
  implicit none

  class(quadrature_xoyoz_type), intent(in)  :: self
  type(function_space_type),    intent(in)  :: function_space
  integer(kind=i_def),          intent(in)  :: function_to_call
  integer(kind=i_def),          intent(in)  :: fspace_dim
  integer(kind=i_def),          intent(in)  :: ndf
  real(kind=r_def),             intent(out) :: basis(fspace_dim,ndf, &
                                                     self%np_x,      &
                                                     self%np_y,      &
                                                     self%np_z)
  basis(:,:,:,:,:) = 0.0_r_def

end subroutine compute_function

end module quadrature_xoyoz_mod
