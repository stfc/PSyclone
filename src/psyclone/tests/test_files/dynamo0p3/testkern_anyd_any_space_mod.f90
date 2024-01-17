! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2019-2024, Science and Technology Facilities Council
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
! Author I. Kavcic, Met Office

module testkern_anyd_any_space_mod

  use constants_mod
  use argument_mod
  use kernel_mod

  implicit none

  ! Description: discontinuous field readwriter (any_discontinuous_space_1)
  ! and continuous readers (any_space_1 and any_w2)
  type, public, extends(kernel_type) :: testkern_anyd_any_space_type
     private
     type(arg_type), dimension(3) :: meta_args = (/                             &
          arg_type(gh_field, gh_real, gh_readwrite, any_discontinuous_space_1), &
          arg_type(gh_field, gh_real, gh_read,      any_space_1),               &
          arg_type(gh_field, gh_real, gh_read,      any_w2)                     &
          /)
     integer :: operates_on = cell_column
   contains
     procedure, public, nopass :: code => testkern_anyd_any_space_code
  end type testkern_anyd_any_space_type

contains

  subroutine testkern_anyd_any_space_code(nlayers, field1, field2, field3,     &
                                          ndf_adspc1, undf_adspc1, map_adspc1, &
                                          ndf_aspc1, undf_aspc1, map_aspc1,    &
                                          ndf_any_w2, undf_any_w2, map_any_w2)


    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_adspc1
    integer(kind=i_def), intent(in) :: ndf_aspc1
    integer(kind=i_def), intent(in) :: ndf_any_w2
    integer(kind=i_def), intent(in) :: undf_adspc1, &
                                       undf_aspc1, undf_any_w2
    integer(kind=i_def), intent(in), dimension(ndf_adspc1) :: map_adspc1
    integer(kind=i_def), intent(in), dimension(ndf_aspc1)  :: map_aspc1
    integer(kind=i_def), intent(in), dimension(ndf_aspc1)  :: map_any_w2
    real(kind=r_def), intent(inout), dimension(undf_adspc1) :: field1
    real(kind=r_def), intent(in), dimension(undf_aspc1)     :: field2
    real(kind=r_def), intent(in), dimension(undf_any_w2)    :: field3

  end subroutine testkern_anyd_any_space_code

end module testkern_anyd_any_space_mod
