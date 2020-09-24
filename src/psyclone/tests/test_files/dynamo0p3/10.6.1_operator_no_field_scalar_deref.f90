!-------------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2020, Science and Technology Facilities Council
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

program operator_example

  use constants_mod,                        only : i_def
  use quadrature_xyoz_mod,                  only : quadrature_xyoz_type
  use quadrature_rule_gaussian_mod,         only : quadrature_rule_gaussian_type
  use testkern_operator_nofield_scalar_mod, only : testkern_operator_nofield_scalar_type

  ! Pretend we have some derived types that we must dereference in order
  ! to get the scalar and operator arguments of the kernel. Note,
  ! "init_quadrature_symmetrical" is an actual constructor method for
  ! this quadrature type in the LFRic code but it is not public. Here we
  ! pretend the method is public so we can mimic the "qr" argument being
  ! obtained by de-referencing.
  type(some_scalar_type)              :: box
  type(some_operator_type)            :: opbox
  type(quadrature_xyoz_type)          :: qr
  type(quadrature_rule_gaussian_type) :: qrl_gauss

  call invoke(testkern_operator_nofield_scalar_type( &
                       opbox%my_mapping, box%b(1),   &
                       qr%init_quadrature_symmetrical(3_i_def, qrl_gauss)))

end program operator_example
