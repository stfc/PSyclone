! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2024, Science and Technology Facilities Council
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
! Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
! Modified: I. Kavcic, Met Office

program multi_kernel_anyspace_stencil

  ! Description: an example where stencils and any_space are used
  ! in different kernels. We check that the same field always has the
  ! same stencil dofmap irrespective of the any_space name.
  use constants_mod,                           only: i_def
  use field_mod,                               only: field_type
  use testkern_same_anyspace_stencil_mod,      only: testkern_same_anyspace_stencil_type
  use testkern_different_anyspace_stencil_mod, only: testkern_different_anyspace_stencil_type

  implicit none

  type(field_type) :: f0, f1, f2, f3
  integer(i_def)   :: extent = 2

  ! 1) Different kernel, same field, same any_space (f1)
  ! 2) Different kernel, same field, different any_space (f2)
  call invoke(                                              &
       testkern_same_anyspace_stencil_type(f0,              &
                                           f1, extent,      &
                                           f2, extent),     &
       testkern_different_anyspace_stencil_type(f3,         &
                                                f1, extent, &
                                                f2, extent) &
       )

end program multi_kernel_anyspace_stencil
