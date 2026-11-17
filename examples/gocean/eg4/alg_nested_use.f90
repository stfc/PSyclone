! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module alg

contains

  subroutine do_update(fld1)
    use field_mod, only: r2d_field
    use kern_nested_use_mod, only: kern_nested_use
    implicit none
    type(r2d_field), intent(inout) :: fld1

    ! Invoke calls a kernel that also calls a kernel defined in
    ! another module but which also imports data from a third module.
    call invoke(kern_nested_use(fld1))

  end subroutine do_update

end module alg
