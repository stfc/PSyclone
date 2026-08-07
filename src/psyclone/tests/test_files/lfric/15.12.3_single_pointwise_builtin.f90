! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation specified in an invoke call.
  use field_mod, only: field_type

  implicit none

  type(field_type) :: f1

  call invoke( setval_c(f1, 0.0) )

end program single_invoke

subroutine expected_code(fld, value)
        do df1 = 1, ndf_w3
           idx = ((cell-1)*nlayers + (k-1))*ndf_w3 + df1
           fld(idx) = value
        end do
end subroutine expected_code
