!-----------------------------------------------------------------------------
! (C) Crown copyright 2018-2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
!-------------------------------------------------------------------------------

! BSD 3-Clause License
!
! Copyright (c) 2020-2024, Science and Technology Facilities Council
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
! Modified by J. Henrichs, Bureau of Meteorology

!> @brief Provides access to the MPI related functionality
!>
!> Provides access to global reduction functions, all_gather, broadcasts and
!. generation of the halo exchange redistribution object. In order for that
!> functionality to work, the subroutine store_comm must first be called to
!> store a valid MPI communicator.
!>
module mpi_mod

  use constants_mod, only : i_def, i_halo_index, i_native, &
                            l_def, r_def, str_def,         &
                            real_type, integer_type,       &
                            r_single, logical_type
  use log_mod,       only : log_event, LOG_LEVEL_ERROR

  implicit none

  private
  public initialise_comm, finalise_comm, store_comm, clear_comm, &
         global_sum, global_min, global_max, &
         all_gather, broadcast, &
         generate_redistribution_map, &
         get_mpi_datatype, &
         get_comm_size, get_comm_rank

  ! JH: Define the values for a single process. This way this
  ! module can be used without special initialisation, which
  ! means the LFRic profiling library can be used in GOcean.
  ! The mpi communicator
  integer(i_def), private :: comm=1, comm_size=1, comm_rank=0
  ! Flag marks whether an MPI communicator has been stored
  logical(l_def), private :: comm_set = .true.

  ! Generic interface for specific broadcast functions
  interface broadcast
   module procedure broadcast_l_def, &
                    broadcast_i_def, &
                    broadcast_r_def, &
                    broadcast_str
  end interface

  ! Generic interface for specific global_sum functions
  interface global_sum
   module procedure global_sum_i_def, &
                    global_sum_r_def, &
                    global_sum_r32_def
  end interface

  ! Generic interface for specific max functions
  interface global_max
   module procedure global_max_i_def, &
                    global_max_r_def, &
                    global_max_r32_def
  end interface

  ! Generic interface for specific min functions
  interface global_min
   module procedure global_min_i_def, &
                    global_min_r_def, &
                    global_min_r32_def
  end interface

contains

  !> Initialises MPI and returns mpi_comm_world as the communicator
  !>
  !> @param out_comm The MPI communicator.
  !>
  subroutine initialise_comm(out_comm)
    implicit none
    integer(i_native), intent(out) :: out_comm

    out_comm = 1
  end subroutine initialise_comm

  !> Stores the MPI communicator in a private variable, ready for later use.
  !>
  !> @param in_comm The MPI communicator to be stored.
  !>
  subroutine store_comm(in_comm)
    implicit none
    integer(i_def), intent(in) :: in_comm

    comm_size = 1
    comm_rank = 0
    comm = in_comm
    comm_set = .true.
  end subroutine store_comm

  !> Finalises MPI
  !>
  subroutine finalise_comm()
    implicit none

    comm = -999
    comm_size = -999
    comm_rank = -999
    comm_set = .false.
  end subroutine finalise_comm

  !> Clears the stored MPI communicator
  !>
  subroutine clear_comm()
    implicit none
    comm = -999
    comm_size = -999
    comm_rank = -999
    comm_set = .false.
  end subroutine clear_comm

  !> Calculates the global sum of a collection of real local sums
  !>
  !> @param l_sum The sum of the reals on the local partition
  !> @param g_sum The calculated global sum
  !>
  subroutine global_sum_r_def(l_sum, g_sum)
    implicit none
    real(r_def), intent(in)  :: l_sum
    real(r_def), intent(out) :: g_sum

    if(comm_set)then
      g_sum = l_sum
    else
      call log_event( &
      'Call to global_sum failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if

  end subroutine global_sum_r_def

  !> Calculates the global sum of a collection of real local sums
  !>
  !> @param l_sum The sum of the reals on the local partition
  !> @param g_sum The calculated global sum
  !>
  subroutine global_sum_r32_def(l_sum, g_sum)
    implicit none
    real(r_single), intent(in)  :: l_sum
    real(r_single), intent(out) :: g_sum

    if(comm_set)then
      g_sum = l_sum
    else
      call log_event( &
      'Call to global_sum failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if

  end subroutine global_sum_r32_def

  !> Calculates the global sum of a collection of integer local sums
  !>
  !> @param l_sum The sum of the integers on the local partition
  !> @param g_sum The calculated global sum
  !>
  subroutine global_sum_i_def(l_sum, g_sum)
    implicit none
    integer(i_def), intent(in)  :: l_sum
    integer(i_def), intent(out) :: g_sum

    if(comm_set)then
      g_sum = l_sum
    else
      call log_event( &
      'Call to global_sum failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if

  end subroutine global_sum_i_def


  !> Calculates the global minimum of a collection of local real minimums
  !>
  !> @param l_min The min on the local partition
  !> @param g_min The calculated global minimum
  !>
  subroutine global_min_r_def(l_min, g_min)
    implicit none
    real(r_def), intent(in)  :: l_min
    real(r_def), intent(out) :: g_min

    if(comm_set)then
      g_min = l_min
    else
      call log_event( &
      'Call to global_min failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if

  end subroutine global_min_r_def

  !> Calculates the global minimum of a collection of local real minimums
  !>
  !> @param l_min The min on the local partition
  !> @param g_min The calculated global minimum
  !>
  subroutine global_min_r32_def(l_min, g_min)
    implicit none
    real(r_single), intent(in)  :: l_min
    real(r_single), intent(out) :: g_min

    if(comm_set)then
      g_min = l_min
    else
      call log_event( &
      'Call to global_min failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if

  end subroutine global_min_r32_def

  !> Calculates the global minimum of a collection of local integer minimums
  !>
  !> @param l_min The min on the local partition
  !> @param g_min The calculated global minimum
  !>
  subroutine global_min_i_def(l_min, g_min)
    implicit none
    integer(i_def), intent(in)  :: l_min
    integer(i_def), intent(out) :: g_min

    if(comm_set)then
      g_min = l_min
    else
      call log_event( &
      'Call to global_min failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if

  end subroutine global_min_i_def


  !> Calculates the global maximum of a collection of local real maximums
  !>
  !> @param l_min The max on the local partition
  !> @param g_max The calculated global maximum
  !>
  subroutine global_max_r_def(l_max, g_max)
    implicit none
    real(r_def), intent(in)  :: l_max
    real(r_def), intent(out) :: g_max

    if(comm_set)then
      g_max = l_max
    else
      call log_event( &
      'Call to global_max failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if

  end subroutine global_max_r_def


  !> Calculates the global maximum of a collection of local real maximums
  !>
  !> @param l_min The max on the local partition
  !> @param g_max The calculated global maximum
  !>
  subroutine global_max_r32_def(l_max, g_max)
    implicit none
    real(r_single), intent(in)  :: l_max
    real(r_single), intent(out) :: g_max

    if(comm_set)then
      g_max = l_max
    else
      call log_event( &
      'Call to global_max failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if

  end subroutine global_max_r32_def

  !> Calculates the global maximum of a collection of local integer maximums
  !>
  !> @param l_min The max on the local partition
  !> @param g_max The calculated global maximum
  !>
  subroutine global_max_i_def(l_max, g_max)
    implicit none
    integer(i_def), intent(in)  :: l_max
    integer(i_def), intent(out) :: g_max

    if(comm_set)then
      g_max = l_max
    else
      call log_event( &
      'Call to global_max failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if

  end subroutine global_max_i_def


  !> Gather integer data from all MPI tasks into a single array in all MPI tasks
  !> The data in send_buffer from the jth process is received by every
  !> process and placed in the jth block of the buffer recv_buffer.
  !>
  !> @param send_buffer The buffer of data to be sent to all MPI tasks
  !> @param recv_buffer The buffer into which the gathered data will be placed
  !> @param count The number of items in send_buffer
  subroutine all_gather(send_buffer, recv_buffer, count)
    implicit none
    integer(i_def), intent(in)  :: send_buffer(:)
    integer(i_def), intent(out) :: recv_buffer(:)
    integer(i_def), intent(in)  :: count

    if(comm_set)then
      recv_buffer(1:count) = send_buffer(1:count)
    else
      call log_event( &
      'Call to all_gather failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if
  end subroutine all_gather



  !> Broadcasts logical data from the root MPI task to all other MPI tasks
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_l_def(buffer, count, root)

    implicit none

    logical(l_def), intent(inout) :: buffer(:)
    integer(i_def), intent(in)    :: count
    integer(i_def), intent(in)    :: root

    if(.not. comm_set)then
      call log_event( &
      'Call to broadcast failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if
  end subroutine broadcast_l_def

  !> Broadcasts integer data from the root MPI task to all other MPI tasks
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_i_def(buffer, count, root)

    implicit none

    integer(i_def), intent(inout) :: buffer(:)
    integer(i_def), intent(in)    :: count
    integer(i_def), intent(in)    :: root

    if(.not. comm_set)then
      call log_event( &
      'Call to broadcast failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if
  end subroutine broadcast_i_def

  !> Broadcasts real data from the root MPI task to all other MPI tasks
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_r_def(buffer, count, root)

    implicit none

    real(r_def),    intent(inout) :: buffer(:)
    integer(i_def), intent(in)    :: count
    integer(i_def), intent(in)    :: root

    if(.not. comm_set)then
      call log_event( &
      'Call to broadcast failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if
  end subroutine broadcast_r_def

  !> Broadcasts character data from the root MPI task to all other MPI tasks
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_str(buffer, count, root)

    implicit none

    character(len=*), intent(inout) :: buffer(:)
    integer(i_def),   intent(in)    :: count
    integer(i_def),   intent(in)    :: root

    if(.not. comm_set)then
      call log_event( &
      'Call to broadcast failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if
  end subroutine broadcast_str



  !> Generate the halo exchange redistribution object to be used for future
  !> halo exchanges
  !>
  !> @param src_indices The global indices of all the owned points in this
  !>                    MPI task
  !> @param tgt_indices The global indices of all the halo points in this
  !>                    MPI task
  !> @param datatype    The MPI datatype of a single element in the data to be
  !>                    exchanged
  !> @return redist     The halo exchange redistribution object
  !>
  function generate_redistribution_map(src_indices, tgt_indices, datatype) &
                                       result(redist)
    implicit none
    integer(i_halo_index), intent(in) :: src_indices(:), tgt_indices(:)
    integer(i_def),        intent(in) :: datatype
    integer(i_native), allocatable :: redist(:)

    allocate(redist(size(src_indices)))
    redist = 1
    return

!    integer(i_def) src_idxlist(:), tgt_idxlist(:)
!    integer(i_def), pointer :: src_offsets(:)
!    integer(i_def), pointer :: tgt_offsets(:)
!    integer(i_def) :: i
!
!    if(comm_set)then
!      ! create decomposition descriptors
!      src_idxlist = xt_idxvec_new( src_indices, size(src_indices) )
!      tgt_idxlist = xt_idxvec_new( tgt_indices, size(tgt_indices) )
!
!      ! generate exchange map
!      xmap = xt_xmap_dist_dir_new(src_idxlist, tgt_idxlist, comm)
!
!      allocate(src_offsets( size(src_indices) ))
!      allocate(tgt_offsets( size(tgt_indices) ))
!
!      src_offsets = (/(i, i = 0, size(src_indices) - 1)/)
!      tgt_offsets = (/(i, i = size(src_indices) , &
!                              size(src_indices) + size(tgt_indices) - 1 )/)
!
!      redist = xt_redist_p2p_off_new(xmap, src_offsets,tgt_offsets, datatype)
!
!      call xt_xmap_delete(xmap)
!      call xt_idxlist_delete(tgt_idxlist)
!      call xt_idxlist_delete(src_idxlist)
!      deallocate(src_offsets)
!      deallocate(tgt_offsets)
!    else
!      call log_event( &
!      'Call to generate_redistribution_map failed. Must call store_comm first',&
!      LOG_LEVEL_ERROR )
!    end if

  end function generate_redistribution_map


  !> Returns the appropriate MPI datatype enumerator for all the Fortran
  !> kinds supported by the LFRic distributed memory code
  !>
  !> @param fortran_type An integer parameter indicating the Fortran data type
  !> @param fortran_kind A Fortran kind variable
  !> @return mpi_datatype The MPI datatype enumerator associated with the
  !>                      given Fortran type and kind
  function get_mpi_datatype( fortran_type, fortran_kind ) result(mpi_datatype)
    implicit none
    integer(i_native), intent(in) :: fortran_type
    integer(i_native), intent(in) :: fortran_kind
    integer(i_native)             :: mpi_datatype
    mpi_datatype = 1
  end function get_mpi_datatype

  !> Returns the number of MPI ranks in the communicator
  !>
  !> @return c_size The number of MPI ranks in the communicator
  function get_comm_size() result(c_size)
    implicit none
    integer(i_def) :: c_size
    if(comm_set)then
      c_size = comm_size
    else
      call log_event( &
      'Call to get_com_size failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if
  end function get_comm_size

  !> Returns the number of the local MPI rank
  !>
  !> @return c_size The number of the local MPI rank
  function get_comm_rank() result(c_rank)
    implicit none
    integer(i_def) :: c_rank
    if(comm_set)then
      c_rank = comm_rank
    else
      call log_event( &
      'Call to get_com_rank failed. Must call store_comm first',&
      LOG_LEVEL_ERROR )
    end if
  end function get_comm_rank

end module mpi_mod
