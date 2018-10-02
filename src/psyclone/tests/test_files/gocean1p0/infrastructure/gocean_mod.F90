!> Module containing basic utilities
module gocean_mod
  use kind_params_mod
  implicit none

  !> Interface to logging routines
  interface model_write_log
     module procedure write_log_a, write_log_ir, &
                      write_log_i, write_log_r
  end interface

contains

  !===================================================

  !> Initialise the GOcean environment
  subroutine gocean_initialise()
#if _OPENACC
    use openacc
#endif
    use parallel_mod, only: parallel_init
    implicit none

    call parallel_init()

#if _OPENACC
    call acc_init(acc_device_nvidia)
#endif
  end subroutine gocean_initialise

  !===================================================

  !> Clean-up the GOcean environment
  subroutine gocean_finalise()
    use parallel_mod, only: parallel_finalise

    call parallel_finalise()

  end subroutine gocean_finalise

  !===================================================

  !> Stop the model run. Passes message on down to parallel_abort().
  !! @param[in] msg Message to print - reason we're stopping
  subroutine gocean_stop(msg)
    use parallel_mod, only: parallel_abort
    implicit none
    character(len=*), intent(in) :: msg

    call parallel_abort(msg)

  end subroutine gocean_stop

  !===================================================

  !> Write log entry with one integer and one real arg
  SUBROUTINE write_log_ir(fmtstr, istep, fvar)
    use iso_fortran_env, only : output_unit ! access computing environment
    IMPLICIT none
    CHARACTER(LEN=*), INTENT(in) :: fmtstr
    INTEGER,          INTENT(in) :: istep
    REAL(go_wp),      INTENT(in) :: fvar

    WRITE(output_unit,FMT=fmtstr) istep, fvar

  END SUBROUTINE write_log_ir

  !===================================================

  !> Write log entry with one integer arg
  SUBROUTINE write_log_i(fmtstr, istep)
    use iso_fortran_env, only : output_unit ! access computing environment
    IMPLICIT none
    CHARACTER(LEN=*), INTENT(in) :: fmtstr
    INTEGER,          INTENT(in) :: istep

    WRITE(output_unit,FMT=fmtstr) istep

  END SUBROUTINE write_log_i

  !===================================================

  !> Write log entry with one real arg
  subroutine write_log_r(fmtstr, fvar)
    use iso_fortran_env, only : output_unit ! access computing environment
    implicit none
    character(len=*), intent(in) :: fmtstr
    real(go_wp),      intent(in) :: fvar

    write(output_unit,FMT=fmtstr) fvar

  end subroutine write_log_r

  !===================================================

  !> Write log entry with just a string
  subroutine write_log_a(fmtstr, msg)
    use iso_fortran_env, only : output_unit ! access computing environment
    implicit none
    character(len=*), intent(in) :: fmtstr
    character(len=*), intent(in) :: msg

    write(output_unit,FMT=fmtstr) msg

  end subroutine write_log_a

  !===================================================

end module gocean_mod
