!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> Manages the extrusion_uniform namelist.
!>
module extrusion_uniform_config_mod

  use constants_mod, only: i_def, &
                           i_native, &
                           r_def, &
                           str_def
  use log_mod,       only: log_event, log_scratch_space &
                         , LOG_LEVEL_ERROR, LOG_LEVEL_WARNING, LOG_LEVEL_INFO

  implicit none

  private
  public :: read_extrusion_uniform_namelist, postprocess_extrusion_uniform_namelist, &
            extrusion_uniform_is_loadable, extrusion_uniform_is_loaded, extrusion_uniform_final

  real(r_def), public, protected :: domain_top
  integer(i_def), public, protected :: number_of_layers

  logical :: namelist_loaded = .false.

contains

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] local_rank Rank of current process.
  !>
  subroutine read_extrusion_uniform_namelist( file_unit, local_rank )

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank

    call read_namelist( file_unit, local_rank )

  end subroutine read_extrusion_uniform_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, local_rank )

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank

    integer(i_def) :: buffer_integer_i_def(1)
    real(r_def) :: buffer_real_r_def(1)

    namelist /extrusion_uniform/ domain_top, &
                                 number_of_layers

    integer(i_native) :: condition

    domain_top = rmdi
    number_of_layers = imdi

    if (local_rank == 0) then

      read( file_unit, nml=extrusion_uniform, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

    end if

    buffer_real_r_def(1) = domain_top
    buffer_integer_i_def(1) = number_of_layers

    domain_top = buffer_real_r_def(1)
    number_of_layers = buffer_integer_i_def(1)

    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_extrusion_uniform_namelist()

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none


  end subroutine postprocess_extrusion_uniform_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function extrusion_uniform_is_loadable()

    implicit none

    logical :: extrusion_uniform_is_loadable

    extrusion_uniform_is_loadable = .not. namelist_loaded

  end function extrusion_uniform_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function extrusion_uniform_is_loaded()

    implicit none

    logical :: extrusion_uniform_is_loaded

    extrusion_uniform_is_loaded = namelist_loaded

  end function extrusion_uniform_is_loaded

  !> Clear out any allocated memory
  !>
  subroutine extrusion_uniform_final()

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none

    domain_top = real(rmdi,r_def)
    number_of_layers = imdi

    return
  end subroutine extrusion_uniform_final


end module extrusion_uniform_config_mod
