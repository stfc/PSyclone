! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

function afunction(iarg) result(num)
  implicit none
  integer :: iarg
  integer :: num
  if(iarg > 0)then
     num = iarg + 1
  else
     num = iarg - 1
  end if
end function afunction
