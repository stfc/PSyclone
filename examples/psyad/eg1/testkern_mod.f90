! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Example module containing a very simple kernel subroutine that
! contains assignments within a loop over array elements.

module testkern_mod
  implicit none

contains

  subroutine testkern_code(ascalar, field1, field2, field3, npts)
    real, intent(in) :: ascalar
    integer, intent(in) :: npts
    real, intent(inout), dimension(npts) :: field2
    ! Deliberately leave off the intent for 'field1' to demonstrate that
    ! it is correctly set in the generated adjoint code.
    real, dimension(npts) :: field1
    ! This argument must be made intent(inout) in the adjoint
    real, intent(in), dimension(npts) :: field3
    ! Locals
    real :: tmp, tmp2, tmp3
    integer :: i

    tmp = ascalar*ascalar
    do i=1,npts
       tmp2 = tmp*i
       field1(i) = tmp*field1(i) + field2(i) + field3(i)
       tmp3 = tmp2*3.0
       field2(i) = field2(i) + field1(i)/tmp2
    end do
    field2(npts) = field2(npts) + field1(1)
    ! The transformation from array notation to loops triggers the
    ! unary minus error captured in issue #1583. This is because the
    ! simplify method reorders the statements and turns the minus into
    ! a unary operator.
    ! field2(:) = field1(:) - tmp*field3(:)
    field2(:) = field1(:) + tmp*field3(:)

  end subroutine testkern_code
  
end module testkern_mod
