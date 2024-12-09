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
! Author: A. R. Porter, STFC Daresbury Lab

module kernel_with_use_mod
  use argument_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod
  implicit none

  private

  public kernel_with_use, kernel_with_use_code

  type, extends(kernel_type) :: kernel_with_use
     type(go_arg), dimension(3) :: meta_args =                 &
          (/ go_arg(GO_READ,      GO_I_SCALAR, GO_POINTWISE),  &
             go_arg(GO_READWRITE, GO_CT,       GO_POINTWISE),  &
             go_arg(GO_READ,                   GO_GRID_MASK_T) &
           /)

     !> Although this is a boundary-conditions kernel, it only
     !! acts on the internal points of the domain
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     integer :: index_offset = GO_OFFSET_NE

  contains
    procedure, nopass :: code => kernel_with_use_code
 end type kernel_with_use

contains
 
  subroutine kernel_with_use_code(ji, jj, istep, ssha, tmask)
    ! rdt is a parameter but magic is not.
    use model_mod, only: rdt, magic
    implicit none
    integer, intent(in)  :: ji, jj
    integer, dimension(:,:),  intent(in)    :: tmask
    integer,                  intent(in)    :: istep
    real(go_wp), dimension(:,:), intent(inout) :: ssha
    ! Locals
    real(go_wp) :: amp_tide, omega_tide, rtime

    amp_tide   = 0.2_go_wp * rdt * magic
    omega_tide = 2.0_go_wp * 3.14159_go_wp / (12.42_go_wp * 3600._go_wp)
    rtime = real(istep) * rdt

    if(tmask(ji,jj) <= 0) return
    IF     (tmask(ji,jj-1) < 0) THEN
       ssha(ji,jj) = amp_tide * sin(omega_tide * rtime)
    ELSE IF(tmask(ji,jj+1) < 0) THEN
       ssha(ji,jj) = amp_tide * sin(omega_tide * rtime)
    ELSE IF(tmask(ji+1,jj) < 0) THEN
       ssha(ji,jj) = amp_tide * sin(omega_tide * rtime)
    ELSE IF(tmask(ji-1,jj) < 0) THEN
       ssha(ji,jj) = amp_tide * sin(omega_tide * rtime)
    END IF

  end subroutine kernel_with_use_code

end module kernel_with_use_mod
