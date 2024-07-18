!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------

! BSD 3-Clause License
!
! Modifications copyright (c) 2020, Science and Technology Facilities Council
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Modified by J. Henrichs, Bureau of Meteorology,
!             I. Kavcic, Met Office

!> Manages the domain_size namelist.
!>
module domain_size_config_mod

  use constants_mod, only: i_native, &
                           r_def
  use log_mod,       only: log_event, log_scratch_space &
                         , LOG_LEVEL_ERROR, LOG_LEVEL_WARNING, LOG_LEVEL_INFO

  implicit none

  private
  public :: read_domain_size_namelist, postprocess_domain_size_namelist, &
            domain_size_is_loadable, domain_size_is_loaded, domain_size_final

  real(r_def), public, protected :: planar_domain_max_x = 1.0
  real(r_def), public, protected :: planar_domain_max_y = 1.0
  real(r_def), public, protected :: planar_domain_min_x = -1.0
  real(r_def), public, protected :: planar_domain_min_y = -1.0

  logical :: namelist_loaded = .false.

contains

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] local_rank Rank of current process.
  !>
  subroutine read_domain_size_namelist( file_unit, local_rank )

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank

    call read_namelist( file_unit, local_rank )

  end subroutine read_domain_size_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, local_rank )

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank

    real(r_def) :: buffer_real_r_def(4)

    namelist /domain_size/ planar_domain_max_x, &
                           planar_domain_max_y, &
                           planar_domain_min_x, &
                           planar_domain_min_y

    integer(i_native) :: condition

    planar_domain_max_x = rmdi
    planar_domain_max_y = rmdi
    planar_domain_min_x = rmdi
    planar_domain_min_y = rmdi

    if (local_rank == 0) then

      read( file_unit, nml=domain_size, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

    end if

    buffer_real_r_def(1) = planar_domain_max_x
    buffer_real_r_def(2) = planar_domain_max_y
    buffer_real_r_def(3) = planar_domain_min_x
    buffer_real_r_def(4) = planar_domain_min_y

    planar_domain_max_x = buffer_real_r_def(1)
    planar_domain_max_y = buffer_real_r_def(2)
    planar_domain_min_x = buffer_real_r_def(3)
    planar_domain_min_y = buffer_real_r_def(4)



    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_domain_size_namelist()

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none


  end subroutine postprocess_domain_size_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function domain_size_is_loadable()

    implicit none

    logical :: domain_size_is_loadable

    domain_size_is_loadable = .not. namelist_loaded

  end function domain_size_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function domain_size_is_loaded()

    implicit none

    logical :: domain_size_is_loaded

    domain_size_is_loaded = namelist_loaded

  end function domain_size_is_loaded

  !> Clear out any allocated memory
  !>
  subroutine domain_size_final()

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none

    planar_domain_max_x = real(rmdi,r_def)
    planar_domain_max_y = real(rmdi,r_def)
    planar_domain_min_x = real(rmdi,r_def)
    planar_domain_min_y = real(rmdi,r_def)

    return
  end subroutine domain_size_final


end module domain_size_config_mod
