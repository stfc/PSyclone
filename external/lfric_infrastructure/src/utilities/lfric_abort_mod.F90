!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Utility for aborting LFRic models

module lfric_abort_mod

  implicit none

contains

  !> @brief     Call abort on the global MPI communicator
  !> If the code is being run with MPI, then mpi_abort should be called.
  !> Currently, the global communicator is used. This would require updating
  !> if support for applications coupled with other MPI applications is required
  !> @param[in] ierr  Error code
  subroutine parallel_abort(ierr)

#ifdef NO_MPI
   ! No "use mpi" in non-mpi build
#else
#ifdef LEGACY_MPI
    use mpi, only : mpi_abort, MPI_COMM_WORLD
#else
    use mpi_f08, only : mpi_abort, MPI_COMM_WORLD
#endif
#endif

    implicit none

    integer, intent(in) :: ierr

    integer :: ierror

#ifdef NO_MPI
    integer, parameter :: EXIT_CODE_ON_ERROR = 1
    stop EXIT_CODE_ON_ERROR
#else
    call mpi_abort(MPI_COMM_WORLD, ierr, ierror)
#endif

  end subroutine parallel_abort

end module lfric_abort_mod
