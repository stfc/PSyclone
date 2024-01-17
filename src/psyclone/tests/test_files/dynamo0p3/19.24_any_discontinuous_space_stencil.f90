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

program single_kernel_any_dscnt_space_stencil
  ! An example where stencils and any_discontinuous_space are used within
  ! a single kernel. We check when any_discontinuous_space is the same and when
  ! it is different. When it is the same we should have the same stencil dofmap
  ! (as all other stencil information is the same) and when it is different we
  ! should have a different stencil dofmap (as we do not know if they are on the
  ! same space).
  ! This also tests the case where we have different fields with the same and
  ! different any_discontinuous_space names in different kernels.
  use constants_mod, only: i_def
  use field_mod,     only: field_type
  use testkern_same_any_dscnt_space_stencil_mod, &
                     only: testkern_same_any_dscnt_space_stencil_type
  use testkern_different_any_dscnt_space_stencil_mod, &
                     only: testkern_different_any_dscnt_space_stencil_type

  implicit none

  type(field_type)          :: f0, f1, f2, f3, f4, f5
  integer(i_def), parameter :: extent = 3

  ! 1) same kernel, different field, same any_discontinuous_space (f1, f2)
  ! 2) same kernel, different field, different any_discontinuous_space (f4, f5)
  ! 3) different kernel, different field, same any_discontinuous_space (f1, f4)
  ! 4) different kernel, different field, different any_discontinuous_space (f2, f5)

  call invoke(                                                     &
       testkern_same_any_dscnt_space_stencil_type(f0,              &
                                                  f1, extent,      &
                                                  f2, extent),     &
       testkern_different_any_dscnt_space_stencil_type(f3,         &
                                                       f4, extent, &
                                                       f5, extent) &
       )

end program single_kernel_any_dscnt_space_stencil
