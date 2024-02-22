!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2020, Science and Technology Facilities Council.
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Handles the loading of namelists.
!
module configuration_mod

  use constants_mod, only : i_native, l_def, str_def, str_max_filename
  use log_mod,       only : log_scratch_space, log_event, LOG_LEVEL_ERROR

  use base_mesh_config_mod, only : read_base_mesh_namelist, &
                                   postprocess_base_mesh_namelist, &
                                   base_mesh_is_loadable, &
                                   base_mesh_is_loaded, &
                                   base_mesh_final
  use domain_size_config_mod, only : read_domain_size_namelist, &
                                     postprocess_domain_size_namelist, &
                                     domain_size_is_loadable, &
                                     domain_size_is_loaded, &
                                     domain_size_final
  use extrusion_uniform_config_mod, only : read_extrusion_uniform_namelist, &
                                           postprocess_extrusion_uniform_namelist, &
                                           extrusion_uniform_is_loadable, &
                                           extrusion_uniform_is_loaded, &
                                           extrusion_uniform_final
  use finite_element_config_mod, only : read_finite_element_namelist, &
                                        postprocess_finite_element_namelist, &
                                        finite_element_is_loadable, &
                                        finite_element_is_loaded, &
                                        finite_element_final
  use partitioning_config_mod, only : read_partitioning_namelist, &
                                      postprocess_partitioning_namelist, &
                                      partitioning_is_loadable, &
                                      partitioning_is_loaded, &
                                      partitioning_final
  use perturbation_bell_config_mod, only : read_perturbation_bell_namelist, &
                                           postprocess_perturbation_bell_namelist, &
                                           perturbation_bell_is_loadable, &
                                           perturbation_bell_is_loaded, &
                                           perturbation_bell_final
  use planet_config_mod, only : read_planet_namelist, &
                                postprocess_planet_namelist, &
                                planet_is_loadable, &
                                planet_is_loaded, &
                                planet_final
  use timestepping_config_mod, only : read_timestepping_namelist, &
                                      postprocess_timestepping_namelist, &
                                      timestepping_is_loadable, &
                                      timestepping_is_loaded, &
                                      timestepping_final

  implicit none

  private
  public :: read_configuration, ensure_configuration, final_configuration

contains

  ! Reads configuration namelists from a file.
  !
  ! [in] filename File holding the namelists.
  !
  ! TODO: Assumes namelist tags come at the start of lines.
  ! TODO: Support "namelist file" namelists which recursively call this
  !       procedure to load other namelist files.
  !
  subroutine read_configuration( filename, local_rank )

    use io_utility_mod, only : open_file, close_file

    implicit none

    character(*), intent(in) :: filename

    integer(i_native), intent(in) :: local_rank

    character(str_def), allocatable :: namelists(:)
    integer(i_native) :: unit = -1

    !local_rank = get_comm_rank()

    if (local_rank == 0) unit = open_file( filename )

    call get_namelist_names( unit, local_rank, namelists )

    call read_configuration_namelists( unit, local_rank, &
                                       namelists, filename )

    if (local_rank == 0) call close_file( unit )

  end subroutine read_configuration

  ! Finds names of all namelists present in file.
  !
  ! [in] unit File holding namelists.
  ! [out] names of namelist in file (in order).
  !
  subroutine get_namelist_names( unit, local_rank, names )

    use io_utility_mod, only : read_line

    implicit none

    integer(i_native),  intent(in)                 :: unit
    integer(i_native),  intent(in)                 :: local_rank
    character(str_def), intent(inout), allocatable :: names(:)

    character(str_def), allocatable :: names_temp(:)
    ! TODO: Buffer is large enough for a fair sized string and a filename.
    !       Ideally it should be dynamically sized for the length of the
    !       incoming data but I'm not sure how best to achieve that at the
    !       moment. #1752
    character(str_def + str_max_filename) :: buffer
    logical(l_def)     :: continue_read
    ! Number of names - technically a scalar but must be defined as a
    ! single element array to be broadcast-able
    integer(i_native)  :: namecount(1)

    namecount = 0
    if (local_rank == 0) then
      text_line_loop: do

        continue_read = read_line( unit, buffer )
        if ( .not. continue_read ) exit text_line_loop

        ! TODO: Assumes namelist tags are at the start of lines. #1753
        !
        if (buffer(1:1) == '&') then
          namecount = namecount + 1
          allocate(names_temp(namecount(1)))
          if (namecount(1) > 1) then
            names_temp(1:namecount(1)-1) = names
          end if
          names_temp(namecount(1)) = trim(buffer(2:))
          call move_alloc(names_temp, names)
        end if
      end do text_line_loop
      rewind(unit)
    end if

    if (local_rank /= 0) then
      allocate(names(namecount(1)))
    end if

  end subroutine get_namelist_names

  ! Checks that the requested namelists have been loaded.
  !
  ! [in]  names List of namelists.
  ! [out] success_mask Marks corresponding namelists as having failed.
  !
  ! [return] Overall success.
  !
  function ensure_configuration( names, success_mask )

    implicit none

    character(*),             intent(in)  :: names(:)
    logical(l_def), optional, intent(out) :: success_mask(:)
    logical(l_def)                        :: ensure_configuration

    integer(i_native) :: i
    logical           :: configuration_found = .True.

    if (present(success_mask) &
        .and. (size(success_mask, 1) /= size(names, 1))) then
      call log_event( 'Arguments "names" and "success_mask" to function' &
                      // '"ensure_configuration" are different shapes',  &
                      LOG_LEVEL_ERROR )
    end if

    ensure_configuration = .True.

    name_loop: do i = 1, size(names)
      select case(trim( names(i) ))
        case ('base_mesh')
          configuration_found = base_mesh_is_loaded()
        case ('domain_size')
          configuration_found = domain_size_is_loaded()
        case ('extrusion_uniform')
          configuration_found = extrusion_uniform_is_loaded()
        case ('finite_element')
          configuration_found = finite_element_is_loaded()
        case ('partitioning')
          configuration_found = partitioning_is_loaded()
        case ('perturbation_bell')
          configuration_found = perturbation_bell_is_loaded()
        case ('planet')
          configuration_found = planet_is_loaded()
        case ('timestepping')
          configuration_found = timestepping_is_loaded()
        case default
          write( log_scratch_space, '(A, A, A)' )          &
               "Tried to ensure unrecognised namelist """, &
               trim(names(i)),                             &
               """ was loaded"
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end select

      ensure_configuration = ensure_configuration .and. configuration_found

      if (present(success_mask)) success_mask(i) = configuration_found

    end do name_loop

  end function ensure_configuration

  subroutine read_configuration_namelists( unit, local_rank, &
                                           namelists, filename )
    implicit none

    integer(i_native),  intent(in) :: unit
    integer(i_native),  intent(in) :: local_rank
    character(str_def), intent(in) :: namelists(:)
    character(*),       intent(in) :: filename

    integer(i_native) :: i

    ! Read the namelists
    do i = 1, size(namelists)
      select case (trim(namelists(i)))
        case ('base_mesh')
          if (base_mesh_is_loadable()) then
            call read_base_mesh_namelist( unit, local_rank )
          else
            write( log_scratch_space, '(A)' )      &
                  "Namelist """//                   &
                  trim(namelists(i))//              &
                  """ can not be read. Too many instances?"
            call log_event( log_scratch_space, LOG_LEVEL_ERROR )
          end if
        case ('domain_size')
          if (domain_size_is_loadable()) then
            call read_domain_size_namelist( unit, local_rank )
          else
            write( log_scratch_space, '(A)' )      &
                  "Namelist """//                   &
                  trim(namelists(i))//              &
                  """ can not be read. Too many instances?"
            call log_event( log_scratch_space, LOG_LEVEL_ERROR )
          end if
        case ('extrusion_uniform')
          if (extrusion_uniform_is_loadable()) then
            call read_extrusion_uniform_namelist( unit, local_rank )
          else
            write( log_scratch_space, '(A)' )      &
                  "Namelist """//                   &
                  trim(namelists(i))//              &
                  """ can not be read. Too many instances?"
            call log_event( log_scratch_space, LOG_LEVEL_ERROR )
          end if
        case ('finite_element')
          if (finite_element_is_loadable()) then
            call read_finite_element_namelist( unit, local_rank )
          else
            write( log_scratch_space, '(A)' )      &
                  "Namelist """//                   &
                  trim(namelists(i))//              &
                  """ can not be read. Too many instances?"
            call log_event( log_scratch_space, LOG_LEVEL_ERROR )
          end if
        case ('partitioning')
          if (partitioning_is_loadable()) then
            call read_partitioning_namelist( unit, local_rank )
          else
            write( log_scratch_space, '(A)' )      &
                  "Namelist """//                   &
                  trim(namelists(i))//              &
                  """ can not be read. Too many instances?"
            call log_event( log_scratch_space, LOG_LEVEL_ERROR )
          end if
        case ('perturbation_bell')
          if (perturbation_bell_is_loadable()) then
            call read_perturbation_bell_namelist( unit, local_rank )
          else
            write( log_scratch_space, '(A)' )      &
                  "Namelist """//                   &
                  trim(namelists(i))//              &
                  """ can not be read. Too many instances?"
            call log_event( log_scratch_space, LOG_LEVEL_ERROR )
          end if
        case ('planet')
          if (planet_is_loadable()) then
            call read_planet_namelist( unit, local_rank )
          else
            write( log_scratch_space, '(A)' )      &
                  "Namelist """//                   &
                  trim(namelists(i))//              &
                  """ can not be read. Too many instances?"
            call log_event( log_scratch_space, LOG_LEVEL_ERROR )
          end if
        case ('timestepping')
          if (timestepping_is_loadable()) then
            call read_timestepping_namelist( unit, local_rank )
          else
            write( log_scratch_space, '(A)' )      &
                  "Namelist """//                   &
                  trim(namelists(i))//              &
                  """ can not be read. Too many instances?"
            call log_event( log_scratch_space, LOG_LEVEL_ERROR )
          end if
        case default
          write( log_scratch_space, '(A)' )        &
                "Unrecognised namelist """//        &
                trim(namelists(i))//                &
                """ found in file "//               &
                trim(filename)
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end select
    end do

    ! Perform post load actions
    do i = 1, size(namelists)
      select case (trim(namelists(i)))
        case ('base_mesh')
          call postprocess_base_mesh_namelist()
        case ('domain_size')
          call postprocess_domain_size_namelist()
        case ('extrusion_uniform')
          call postprocess_extrusion_uniform_namelist()
        case ('finite_element')
          call postprocess_finite_element_namelist()
        case ('partitioning')
          call postprocess_partitioning_namelist()
        case ('perturbation_bell')
          call postprocess_perturbation_bell_namelist()
        case ('planet')
          call postprocess_planet_namelist()
        case ('timestepping')
          call postprocess_timestepping_namelist()
        case default
          write( log_scratch_space, '(A)' )        &
                "Unrecognised namelist """//        &
                trim(namelists(i))//                &
                """ found in file "//               &
                trim(filename)
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end select
    end do

  end subroutine read_configuration_namelists

  subroutine final_configuration()

    implicit none

    call base_mesh_final()
    call domain_size_final()
    call extrusion_uniform_final()
    call finite_element_final()
    call partitioning_final()
    call perturbation_bell_final()
    call planet_final()
    call timestepping_final()

    return
  end subroutine final_configuration

end module configuration_mod
