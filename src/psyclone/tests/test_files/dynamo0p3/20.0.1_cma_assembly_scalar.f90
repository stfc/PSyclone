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
! Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
! Modified: I. Kavcic, Met Office

program single_invoke_cma

  ! Description: single CMA assembly kernel which includes a scalar
  ! argument
  use constants_mod,                       only: r_def
  use operator_mod,                        only: operator_type
  use columnwise_operator_mod,             only: columnwise_operator_type
  use columnwise_op_asm_kernel_scalar_mod, only: columnwise_op_asm_kernel_scalar_type

  implicit none

  type(operator_type)            :: lma_op1
  type(columnwise_operator_type) :: cma_op1
  real(kind=r_def)               :: my_scalar = 1.0_r_def

  call invoke( &
          ! Perversely name the scalar argument so as to deliberately clash
          ! with what we might expect to generate for the name of the 'alpha'
          ! parameter of the CMA argument.
          columnwise_op_asm_kernel_scalar_type(lma_op1, cma_op1, cma_op1_alpha) &
          )

end program single_invoke_cma
