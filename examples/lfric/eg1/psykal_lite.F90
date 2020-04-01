!-----------------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
! -----------------------------------------------------------------------------
!
! BSD 3-Clause License
!
! Modifications copyright (c) 2017-2020, Science and Technology Facilities Council
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
! Modified, I. Kavcic, Met Office
!
!-------------------------------------------------------------------------------

!> @brief Provides access to the members of the psy class.

!> @details Accessor functions for the psy class are defined in this module.

!> @param invoke_RHS_V3              Invoke the RHS for a v3 field
!> @param invoke_v3_solver_kernel    Invoke the solver for a v3 field kernel

module psy

use lfric

implicit none

contains

  subroutine invoke_rhs_v3(rhs)
    use v3_kernel_mod,        only : rhs_v3_code
    implicit none
    type(field_type), intent(in) :: rhs
    integer :: cell
    integer, pointer :: map(:)
    integer :: nlayers
    integer :: ndf
    real(kind=dp), pointer  :: v3_basis(:,:,:,:,:)

! Unpack data
    nlayers=rhs%get_nlayers()
    ndf = rhs%vspace%get_ndf()

    call rhs%vspace%get_basis(v3_basis)
    do cell = 1, rhs%get_ncell()
       call rhs%vspace%get_cell_dofmap(cell,map)
       call rhs_v3_code(nlayers,ndf,map,v3_basis,rhs%data,rhs%gaussian_quadrature)
    end do

  end subroutine invoke_rhs_v3

  subroutine invoke_v3_solver_kernel(pdfield,rhs)
    use v3_solver_kernel_mod, only : solver_v3_code
    implicit none
    type(field_type), intent(in) :: pdfield
    type(field_type), intent(in) :: rhs
    integer :: cell
    integer, pointer :: map(:)
    integer :: nlayers
    integer :: ndf
    real(kind=dp), pointer  :: v3_basis(:,:,:,:,:)

    nlayers=pdfield%get_nlayers()
    ndf = pdfield%vspace%get_ndf()
    call pdfield%vspace%get_basis(v3_basis)

    do cell = 1, pdfield%get_ncell()
       call pdfield%vspace%get_cell_dofmap(cell,map)
       call solver_v3_code(nlayers,ndf,map,v3_basis,pdfield%data,rhs%data,pdfield%gaussian_quadrature)
    end do

  end subroutine invoke_v3_solver_kernel

end module psy
