! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2018-2024, Science and Technology Facilities Council.
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
! Author A. R. Porter, STFC Daresbury Lab
! Modified by R. W. Ford, STFC Daresbury Lab

SUBROUTINE tra_ldf_iso()
  USE some_mod, only: dia_ptr_hst
  INTEGER, PARAMETER :: jpi=2, jpj=2, jpk=2
  INTEGER :: jn
  INTEGER, PARAMETER :: wp=4
  LOGICAL :: l_ptr
  INTEGER, DIMENSION(jpi,jpj) :: tmask
  REAL(wp), DIMENSION(jpi,jpj,jpk) ::   zdit, zdjt, zftu, zftv, ztfw
  zftv(:,:,:) = 0.0d0
  IF( l_ptr )  CALL dia_ptr_hst( jn, 'ldf', -zftv(:,:,:)  )
  CALL dia_ptr_hst( jn, 'ldf', -zftv(:,:,:)  )
  zftu(:,:,1) = 1.0d0
  tmask(:,:) = jpi
end SUBROUTINE tra_ldf_iso
