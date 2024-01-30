!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2023, Science and Technology Facilities
! Council
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
! Modified by: I. Kavcic, Met Office
!
!> @brief A module providing r_bl field related classes.
!>
!> @details This is a version of a field object that can hold r_bl data
!> values. It contains both a representation of an r_bl field which provides
!> no access to the underlying data (to be used in the algorithm layer) and an
!> accessor class (to be used in the Psy layer) are provided.


module r_bl_field_mod

  ! Eventually the precision of the field data will be set in a module held
  ! within the model (as it is model information). For now, PSyclone is
  ! expecting to "use" the definitions from field_mod, so it is set here
#if (R_BL_PRECISION == 32)
  use field_r32_mod, only: r_bl_field_type         => field_r32_type, &
                           r_bl_field_proxy_type   => field_r32_proxy_type, &
                           r_bl_field_pointer_type => field_r32_pointer_type
#else
  use field_r64_mod, only: r_bl_field_type         => field_r64_type, &
                           r_bl_field_proxy_type   => field_r64_proxy_type, &
                           r_bl_field_pointer_type => field_r64_pointer_type
#endif

  implicit none
  private

  public :: r_bl_field_type, &
            r_bl_field_proxy_type, &
            r_bl_field_pointer_type

end module r_bl_field_mod
