










!-------------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-------------------------------------------------------------------------------
!> @brief Calls to compiler traceback functionality to provide a backtrace
!>
!> A number of compilers have compiler specific functions to provide backtraces
!> the traceback subroutine here will call the correct function depending on the
!> compiler. Given no function available with a compiler the subroutine will be
!> empty.
module traceback_mod


implicit none

  private
  public traceback

contains

  !> @brief Calls the compiler specific backtrace function.
  !>
  subroutine traceback()

    implicit none


  end subroutine traceback

end module traceback_mod
