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
! Author R. Ford, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

program multikernel_invokes_452

  ! Multiple kernel calls within an invoke where the arguments are specified
  ! as any_space

  use constants_mod,            only : i_def, r_def
  use field_mod,                only : field_type
  use operator_mod,             only : operator_type
  use quadrature_xyoz_mod,      only : quadrature_xyoz_type
  use testkern_any_space_1_mod, only : testkern_any_space_1_type
  use testkern_any_space_2_mod, only : testkern_any_space_2_type
  use testkern_any_space_3_mod, only : testkern_any_space_3_type
  use testkern_any_space_4_mod, only : testkern_any_space_4_type

  implicit none

  type(field_type)           :: f1, f2, f3(3)
  type(operator_type)        :: op, op2, op3, op4, op5
  type(quadrature_xyoz_type) :: qr
  integer(i_def)             :: scalar
  real(r_def)                :: rdt

  call invoke(                                             &
       ! any1, any2, W0
       testkern_any_space_1_type(f1, rdt, f2, f3, qr),     &
       ! any1, any1, any1-any1
       testkern_any_space_2_type(f1, f2, op, scalar),      &
       ! any1, any1, any1-any1
       testkern_any_space_2_type(f2, f1, op, scalar),      &
       ! any1-any2
       testkern_any_space_3_type(op),                      &
       ! any5, any1-any2, any3-any2, any4-any4, any3-any5, any4
       testkern_any_space_4_type(f2,op2,op3,op4,op5,f1,qr) &
       )

end program multikernel_invokes_452
