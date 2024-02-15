! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
! Author: R. W. Ford, STFC Daresbury Laboratory
! Modified: I. Kavcic, Met Office

! Example of calling the same kernel with data in the algorithm layer
! that might use different precision. Scalars, fields and operators
! are provided as an illustration. Mixed case is also used for
! variables to check that this does not affect the generated code.

program mixed_precision

  use constants_mod,         only : r_def, r_solver, r_tran, r_bl, r_phys
  use field_mod,             only : field_type
  use r_solver_field_mod,    only : r_solver_field_type
  use r_tran_field_mod,      only : r_tran_field_type
  use r_bl_field_mod,        only : r_bl_field_type
  use r_phys_field_mod,      only : r_phys_field_type
  use operator_mod,          only : operator_type
  use r_solver_operator_mod, only : r_solver_operator_type
  use r_tran_operator_mod,   only : r_tran_operator_type
  use mixed_kernel_mod,      only : mixed_kernel_type

  real(r_def)                  :: Scalar_r_def
  real(r_solver)               :: sCalar_r_solver
  real(r_tran)                 :: scAlar_r_tran
  real(r_bl)                   :: scaLar_r_bl
  real(r_phys)                 :: scalAr_r_phys
  type(field_type)             :: fieLd_r_def
  type(r_solver_field_type)    :: fielD_r_solver
  type(r_tran_field_type)      :: field_r_tran
  type(r_bl_field_type)        :: fIeld_r_bl
  type(r_phys_field_type)      :: fiEld_r_phys
  type(operator_type)          :: operator_r_def
  type(r_solver_operator_type) :: operator_r_solver
  type(r_tran_operator_type)   :: OperatoR_r_tran

  call invoke(mixed_kernel_type(scalar_r_deF, field_R_def, opeRator_r_def),          &
              mixed_kernel_type(scalar_r_solver, field_r_solver, operator_r_solver), &
              mixed_kernel_type(scalar_r_tran, field_r_tran, OperatoR_r_tran),       &
              mixed_kernel_type(scaLar_r_bl, fIeld_r_bl, OperatoR_r_tran),           &
              mixed_kernel_type(scaLar_r_phys, fIeld_r_phys, opeRator_r_def))

end program mixed_precision
