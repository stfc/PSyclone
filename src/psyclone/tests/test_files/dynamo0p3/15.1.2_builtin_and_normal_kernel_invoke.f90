! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2024, Science and Technology Facilities Council.
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

program single_invoke_builtin_then_kernel

  ! Description: single invoke call with a builtin followed by a kernel call
  use constants_mod,        only: r_def
  use field_mod,            only: field_type
  use testkern_mod,         only: testkern_type
  use testkern_wtheta_mod,  only: testkern_wtheta_type
  use testkern_w2_only_mod, only: testkern_w2_only_type

  implicit none

  type(field_type) :: f1, f2, f3, f4, f5
  real(r_def)      :: scalar = 0.0
  
  call invoke(                               &
       setval_c(f5, 0.0),                    &
       setval_c(f2, 0.0),                    &
       ! f3 function space w2, inc
       ! f2 function space w2, read
       testkern_w2_only_type(f3, f2),        &
       ! f4 function space wtheta, write
       ! f5 function space any_discontinuous_space_1, read
       testkern_wtheta_type(f4, f5),         &
       ! scalar, read
       ! f1 function space w1, inc
       ! f2 function space w2, read
       ! f3 function space w2, read
       ! f4 function space w3, read
       testkern_type(scalar, f1, f2, f3, f4) &
          )

end program single_invoke_builtin_then_kernel
