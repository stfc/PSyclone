! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-20 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module alg

contains

  subroutine do_update(fld2)
    use field_mod, only: r2d_field
    use kern_call_kern_mod, only: kern_call_kern
    implicit none
    type(r2d_field), intent(inout) :: fld2

    ! Invoke calls kernel that itself calls a kernel defined in
    ! another module
    call invoke(kern_call_kern(fld2))

  end subroutine do_update

end module alg
