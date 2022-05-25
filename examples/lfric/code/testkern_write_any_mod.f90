! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2022, Science and Technology Facilities Council
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
! Author: R. W. Ford, STFC Daresbury Lab
! Modified: I. Kavcic, Met Office
!           A. R. Porter, STFC Daresbury Lab

! A kernel which writes to a field on 'any_space'. Such a kernel must guarantee
! that any writes to a given shared entity are for the same value and that the
! first access to such an entity is a write.
module testkern_write_any_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  ! Description: function spaces with one continuous ('any_space_1')
  ! field writer.
  type, extends(kernel_type) :: testkern_write_any_type
     type(arg_type), dimension(2) :: meta_args = (/                        &
          arg_type(gh_field, gh_real, gh_write, any_space_1),              &
          arg_type(gh_field, gh_real, gh_read,  w2)                        &
          /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_write_any_code
  end type testkern_write_any_type

contains

  subroutine testkern_write_any_code(nlayers,                             &
                                     field1, field2,                      &
                                     ndf_aspc1, undf_aspc1, map_aspc1,    &
                                     ndf_w2, undf_w2, map_w2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_aspc1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: undf_aspc1, undf_w2
    integer(kind=i_def), intent(in), dimension(ndf_aspc1)  :: map_aspc1
    integer(kind=i_def), intent(in), dimension(ndf_w2)     :: map_w2
    real(kind=r_def), intent(out), dimension(undf_aspc1)   :: field1
    real(kind=r_def), intent(in), dimension(undf_w2)       :: field2

  end subroutine testkern_write_any_code

end module testkern_write_any_mod
