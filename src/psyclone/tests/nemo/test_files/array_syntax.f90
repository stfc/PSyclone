! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

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
