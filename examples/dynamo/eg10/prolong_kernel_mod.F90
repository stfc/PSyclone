!-----------------------------------------------------------------------------
! (C) Crown copyright 2018 Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
! -----------------------------------------------------------------------------
!
! BSD 3-Clause License
!
! Modifications copyright (c) 2018-2019, Science and Technology Facilities Council
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
! Modified A. R. Porter, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

module prolong_kernel_mod

use constants_mod,           only: i_def, r_def
use kernel_mod,              only: kernel_type
use argument_mod,            only: arg_type,                     &
                                   GH_FIELD,                     &
                                   GH_READ, GH_READWRITE, CELLS, &
                                   ANY_DISCONTINUOUS_SPACE_1,    &
                                   ANY_DISCONTINUOUS_SPACE_2,    &
                                   GH_COARSE, GH_FINE

implicit none

type, public, extends(kernel_type) :: prolong_kernel_type
   private
   type(arg_type) :: meta_args(2) = (/                                                   &
        arg_type(GH_FIELD, GH_READWRITE, ANY_DISCONTINUOUS_SPACE_1, mesh_arg=GH_FINE),   &
        arg_type(GH_FIELD, GH_READ,      ANY_DISCONTINUOUS_SPACE_2, mesh_arg=GH_COARSE ) &
        /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass :: prolong_kernel_code
end type prolong_kernel_type

public :: prolong_kernel_code

contains

  subroutine prolong_kernel_code(nlayers,       &
                                 cell_map,      &
                                 ncell_f_per_c, &
                                 ncell_f,       &
                                 fine,          &
                                 coarse,        &
                                 ndf,           &
                                 undf_f,        &
                                 dofmap_f,      &
                                 undf_c,        &
                                 dofmap_c)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_f_per_c
    integer(kind=i_def), dimension(ncell_f_per_c), intent(in) :: cell_map
    integer(kind=i_def), intent(in) :: ncell_f
    integer(kind=i_def), intent(in) :: ndf
    integer(kind=i_def), dimension(ndf, ncell_f), intent(in) :: dofmap_f
    integer(kind=i_def), dimension(ndf), intent(in) :: dofmap_c
    integer(kind=i_def), intent(in) :: undf_f, undf_c
    real(kind=r_def), dimension(undf_c), intent(in)    :: coarse
    real(kind=r_def), dimension(undf_f), intent(inout) :: fine

    integer(kind=i_def) :: df, k, lp

    do k = 0, nlayers-1
       do df = 1, ndf
          do lp = 1, ncell_f_per_c
             fine(dofmap_f(df,cell_map(lp))+k) &
                  = fine(dofmap_f(df,cell_map(lp))+k) + coarse(dofmap_c(df)+k)
          end do
       end do
    end do

  end subroutine prolong_kernel_code

end module prolong_kernel_mod
