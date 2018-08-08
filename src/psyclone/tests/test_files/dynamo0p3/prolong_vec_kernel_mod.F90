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
! Modifications copyright (c) 2018, Science and Technology Facilities Council
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
! Modified, A. R. Porter, STFC Daresbury Lab

module prolong_vec_kernel_mod
  use argument_mod
  use kernel_mod
  use constants_mod
  type, extends(kernel_type) :: prolong_vec_kernel_type
     type(arg_type), dimension(2) :: meta_args =                  &
          (/ arg_type(gh_field*3,gh_write,w1, mesh_arg=gh_fine),  &
             arg_type(gh_field*3,gh_read, w2, mesh_arg=gh_coarse) &
           /)
     integer :: iterates_over = cells
   contains
     procedure, nopass :: code => prolong_vec_kernel_code
  end type prolong_vec_kernel_type
contains

  subroutine prolong_vec_kernel_code(nlayers,                      &
                                 cell_map, ncell_f_per_c, ncell_f, &
                                 fld1_1, fld1_2, fld1_3,           &
                                 fld2_1, fld2_2, fld2_3,           &
                                 ndf_w1, undf_w1, dofmap_w1,       &
                                 undf_w2, dofmap_w2)
    integer, intent(in) :: nlayers, ncell_f_per_c, ncell_f
    real(kind=r_def), dimension(:) :: fld1_1, fld1_2, fld1_3, &
                                      fld2_1, fld2_2, fld2_3
    integer :: ndf_w1, undf_w1, undf_w2
    integer, dimension(:) :: cell_map, dofmap_w2
    integer, dimension(:,:) :: dofmap_w1

  end subroutine prolong_vec_kernel_code
end module prolong_vec_kernel_mod
