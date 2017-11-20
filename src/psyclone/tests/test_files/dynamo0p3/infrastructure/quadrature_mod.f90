!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
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
! Author R. Ford and A. R. Porter, STFC Daresbury Lab
module quadrature_mod
  use constants_mod, only: r_def

  type quadrature_type
     integer :: nqp_h

   contains
     procedure :: get_xqp_h
     procedure :: get_xqp_v
     procedure :: get_wqp_h
     procedure :: get_wqp_v
     procedure :: get_nqp_v
     procedure :: get_nqp_h

  end type quadrature_type

contains

  function get_xqp_h(self) result(xqp_h)
    implicit none
    class(quadrature_type), target, intent(in) :: self
    real(kind=r_def), pointer :: xqp_h(:,:)

    xqp_h => null()
  end function get_xqp_h

  function get_xqp_v(self) result(xqp_v)
    implicit none
    class(quadrature_type), target, intent(in) :: self
    real(kind=r_def), pointer :: xqp_v(:)

    xqp_v => null()
  end function get_xqp_v

  function get_nqp_v(self) result(nqp_v)
    implicit none
    class(quadrature_type), intent(in) :: self
    integer :: nqp_v

    nqp_v = 0
  end function get_nqp_v

  function get_nqp_h(self) result(nqp_h)
    implicit none
    class(quadrature_type), intent(in) :: self
    integer :: nqp_h

    nqp_h = 0
  end function get_nqp_h

  function get_wqp_h(self) result(wqp_h)
    implicit none
    class(quadrature_type), target, intent(in) :: self
    real(kind=r_def), pointer :: wqp_h(:) 

    wqp_h => null()
  end function get_wqp_h

  function get_wqp_v(self) result(wqp_v)
    implicit none
    class(quadrature_type), target, intent(in) :: self
    real(kind=r_def), pointer :: wqp_v(:) 

    wqp_v => null()
  end function get_wqp_v

end module quadrature_mod
