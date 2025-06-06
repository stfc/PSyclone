! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2025, Science and Technology Facilities Council.
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
! Author: A. R. Porter, STFC Daresbury Lab

program single_invoke_builtin_then_kernel

  ! Description: single invoke call with a builtin followed by a kernel call
  ! with an operator argument.
  use constants_mod,        only: r_def
  use field_mod,            only: field_type
  use operator_mod,         only: operator_type
  use dg_matrix_vector_kernel_mod, only: dg_matrix_vector_kernel_type

  implicit none

  type(field_type) :: f2, f4
  real(r_def)      :: scalar = 0.0
  type(operator_type), pointer    :: mass_matrix => null()

  call invoke(                               &
       setval_c(f2, 0.0),                    &
       ! f4 - discont. space, written
       ! f2 - continuous space, read - needs halo exchange to get
       ! clean annexed dofs.
       dg_matrix_vector_kernel_type(f4, f2, mass_matrix) )

end program single_invoke_builtin_then_kernel
