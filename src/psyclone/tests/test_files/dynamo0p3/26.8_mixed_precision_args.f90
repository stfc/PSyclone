! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2021-2022, Science and Technology Facilities Council.
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

! Example of calling the same kernel with data in the algorithm layer
! that might use different precision. Scalars, fields and operators
! are provided as an illustration. Mixed case is also used for
! variables to check that this does not affect the generated code.

program mixed_precision

  use constants_mod, only : r_def, r_solver, r_tran
  use mixed_mod, only : mixed_type

  real(r_def)                  :: Scalar_r_def
  real(r_solver)               :: sCalar_r_solver
  real(r_tran)                 :: scAlar_r_tran
  type(field_type)             :: fieLd_r_def
  type(r_solver_field_type)    :: fielD_r_solver
  type(r_tran_field_type)      :: field_r_tran
  type(operator_type)          :: operator_r_def
  type(r_solver_operator_type) :: operator_r_solver

  call invoke(mixed_type(scalar_r_deF, field_R_def, opeRator_r_def),          &
              mixed_type(scalar_r_solver, field_r_solver, operator_r_solver), &
              mixed_type(scalar_r_tran, field_r_tran, operator_r_def))

end program mixed_precision
