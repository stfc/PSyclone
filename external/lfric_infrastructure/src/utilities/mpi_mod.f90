










!-----------------------------------------------------------------------------
! (C) Crown copyright 2018 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief Provides access to the MPI related functionality
!>
!> Provides access to global reduction functions, all_gather and broadcasts
!>
module mpi_mod

  use, intrinsic :: iso_fortran_env, only : int32, real32, real64

  use constants_mod, only : str_def, real_type, integer_type, logical_type
  ! No "use mpi" in non-mpi build
  use log_mod,       only : log_event, LOG_LEVEL_ERROR

  implicit none

  private

  public global_mpi, create_comm, destroy_comm, get_mpi_datatype

  type, public :: mpi_type

    private

    !> The mpi communicator
    integer :: comm=-999, comm_size=-999, comm_rank=-999
    !> Flag marks whether an MPI communicator has been stored
    logical :: comm_set = .false.

  contains

    procedure, public :: initialise
    procedure, public :: finalise
    procedure, public :: get_comm
    procedure, public :: is_comm_set
    procedure, public :: global_sum_int32
    procedure, public :: global_sum_real64
    procedure, public :: global_sum_real32
    generic :: global_sum => global_sum_int32,  &
                             global_sum_real64, &
                             global_sum_real32
    procedure, public :: global_min_int32
    procedure, public :: global_min_real64
    procedure, public :: global_min_real32
    generic :: global_min => global_min_int32,  &
                             global_min_real64, &
                             global_min_real32
    procedure, public :: global_max_int32
    procedure, public :: global_max_real64
    procedure, public :: global_max_real32
    generic :: global_max => global_max_int32,  &
                             global_max_real64, &
                             global_max_real32
    procedure, public :: all_gather
    procedure, public :: broadcast_logical_1d
    procedure, public :: broadcast_int32_1d
    procedure, public :: broadcast_real64_1d
    procedure, public :: broadcast_real32_1d
    procedure, public :: broadcast_str_1d
    procedure, public :: broadcast_logical_2d
    procedure, public :: broadcast_int32_2d
    procedure, public :: broadcast_real64_2d
    procedure, public :: broadcast_real32_2d
    procedure, public :: broadcast_logical_3d
    procedure, public :: broadcast_int32_3d
    procedure, public :: broadcast_real64_3d
    procedure, public :: broadcast_real32_3d
    generic :: broadcast => broadcast_logical_1d, &
                            broadcast_int32_1d,   &
                            broadcast_real64_1d,  &
                            broadcast_real32_1d,  &
                            broadcast_str_1d,     &
                            broadcast_logical_2d, &
                            broadcast_int32_2d,   &
                            broadcast_real64_2d,  &
                            broadcast_real32_2d,  &
                            broadcast_logical_3d, &
                            broadcast_int32_3d,   &
                            broadcast_real64_3d,  &
                            broadcast_real32_3d
    procedure, public :: get_comm_size
    procedure, public :: get_comm_rank

  end type mpi_type

  !Global MPI object
  !> @todo This needs to be moved out of global scope and into the modeldb
  !>       object, when that object exists
  type(mpi_type), target :: global_mpi

contains

  !> Helper function (not type bound) that creates a communticator
  !> by initialsing mpi and returning mpi_comm_world
  !>
  !> @param out_comm An MPI communicator.
  !>
  subroutine create_comm(out_comm)
    implicit none
    integer, intent(out) :: out_comm
    integer :: ierr

    ! Don't initialise mpi in non-mpi build.
    out_comm = 0
    ierr=0 ! Set local variable to avoid unused variable errors
  end subroutine create_comm

  !> Helper function (not type bound) that finalises mpi and releases
  !> mpi_comm_world
  !>
  subroutine destroy_comm()
    implicit none
    integer :: ierr

    ! Don't finalise mpi in non-mpi build
    ierr=0 ! Set local variable to avoid unused variable errors
  end subroutine destroy_comm

  !>  Helper function (not type bound) that returns the appropriate
  !>  MPI datatype enumerator for all the Fortran kinds supported by the
  !>  LFRic distributed memory code
  !>
  !> @param fortran_type An integer parameter indicating the Fortran data type
  !> @param fortran_kind A Fortran kind variable
  !> @return mpi_datatype The MPI datatype enumerator associated with the
  !>                      given Fortran type and kind
  function get_mpi_datatype( fortran_type, fortran_kind ) result(mpi_datatype)
    use, intrinsic :: iso_fortran_env, only : real128, real64, real32, &
                                              int64, int32, int16, int8
    implicit none
    integer, intent(in) :: fortran_type
    integer, intent(in) :: fortran_kind
    integer             :: mpi_datatype

    ! In a non-mpi build the mpi datatype is meaningless - just return zero
    mpi_datatype = 0

  end function get_mpi_datatype

  !> Stores the MPI communicator in a private variable, ready for later use.
  !>
  !> @param in_comm The MPI communicator to be stored.
  !>
  subroutine initialise(self, in_comm)
    implicit none
    class(mpi_type), intent(inout) :: self
    integer,         intent(in)    :: in_comm
    integer :: ierr

    self%comm = in_comm
    ! Set default values for number of ranks and local rank in non-mpi build
    self%comm_size = 1
    self%comm_rank = 0
    ierr=0 ! Set local variable to avoid unused variable errors
    self%comm_set = .true.
  end subroutine initialise

  !> Finalises the MPI object
  !>
  subroutine finalise(self)
    implicit none
    class(mpi_type), intent(inout) :: self

    self%comm = -999
    self%comm_size = -999
    self%comm_rank = -999
    self%comm_set = .false.
  end subroutine finalise

  !> Returns the stored MPI communicator
  !> @return communicator The stored MPI communicator
  !>
  function get_comm(self) result(communicator)
    implicit none
    class(mpi_type), intent(in) :: self
    integer :: communicator
    communicator = self%comm
  end function get_comm

  !> Returns whether the MPI communicator has been stored
  !> @return comm_state A flag indicating whether the MPI communicator is stored
  !>
  function is_comm_set(self) result(comm_state)
    implicit none
    class(mpi_type), intent(inout) :: self
    logical :: comm_state
    comm_state = self%comm_set
  end function is_comm_set

  !> Calculates the global sum of a collection of real local sums
  !>
  !> @param l_sum The sum of the reals on the local partition
  !> @param g_sum The calculated global sum
  !>
  subroutine global_sum_real64(self, l_sum, g_sum)
    implicit none
    class(mpi_type), intent(inout) :: self
    real(real64), intent(in)       :: l_sum
    real(real64), intent(out)      :: g_sum

    integer :: err

    ! Global sum and local sum are the same thing in a non-mpi build
    g_sum = l_sum
    err=0 ! Set local variable to avoid unused variable errors

  end subroutine global_sum_real64


  !> Calculates the global sum of a collection of real32 local sums
  !>
  !> @param l_sum The sum of the reals on the local partition
  !> @param g_sum The calculated global sum
  !>
  subroutine global_sum_real32(self, l_sum, g_sum)
    implicit none
    class(mpi_type), intent(inout) :: self
    real(real32), intent(in)       :: l_sum
    real(real32), intent(out)      :: g_sum

    integer :: err

    ! Global sum and local sum are the same thing in a non-mpi build
    g_sum = l_sum
    err=0 ! Set local variable to avoid unused variable errors

  end subroutine global_sum_real32


  !> Calculates the global sum of a collection of integer local sums
  !>
  !> @param l_sum The sum of the integers on the local partition
  !> @param g_sum The calculated global sum
  !>
  subroutine global_sum_int32(self, l_sum, g_sum)
    implicit none
    class(mpi_type), intent(inout) :: self
    integer(int32), intent(in)     :: l_sum
    integer(int32), intent(out)    :: g_sum

    integer :: err

    ! Global sum and local sum are the same thing in a non-mpi build
    g_sum = l_sum
    err=0 ! Set local variable to avoid unused variable errors

  end subroutine global_sum_int32


  !> Calculates the global minimum of a collection of local real minimums
  !>
  !> @param l_min The min on the local partition
  !> @param g_min The calculated global minimum
  !>
  subroutine global_min_real64(self, l_min, g_min)
    implicit none
    class(mpi_type), intent(inout) :: self
    real(real64), intent(in)       :: l_min
    real(real64), intent(out)      :: g_min

    integer :: err

    ! Global minimum and local minimum are the same thing in a non-mpi build
    g_min = l_min
    err=0 ! Set local variable to avoid unused variable errors

  end subroutine global_min_real64

  !> Calculates the global minimum of a collection of local real32 minimums
  !>
  !> @param l_min The min on the local partition
  !> @param g_min The calculated global minimum
  !>
  subroutine global_min_real32(self, l_min, g_min)
    implicit none
    class(mpi_type), intent(inout) :: self
    real(real32), intent(in)       :: l_min
    real(real32), intent(out)      :: g_min

    integer :: err

    ! Global minimum and local minimum are the same thing in a non-mpi build
    g_min = l_min
    err=0 ! Set local variable to avoid unused variable errors

  end subroutine global_min_real32


  !> Calculates the global minimum of a collection of local integer minimums
  !>
  !> @param l_min The min on the local partition
  !> @param g_min The calculated global minimum
  !>
  subroutine global_min_int32(self, l_min, g_min)
    implicit none
    class(mpi_type), intent(inout) :: self
    integer(int32), intent(in)     :: l_min
    integer(int32), intent(out)    :: g_min

    integer :: err

    ! Global minimum and local minimum are the same thing in a non-mpi build
    g_min = l_min
    err=0 ! Set local variable to avoid unused variable errors

  end subroutine global_min_int32


  !> Calculates the global maximum of a collection of local real maximums
  !>
  !> @param l_min The max on the local partition
  !> @param g_max The calculated global maximum
  !>
  subroutine global_max_real64(self, l_max, g_max)
    implicit none
    class(mpi_type), intent(inout) :: self
    real(real64), intent(in)       :: l_max
    real(real64), intent(out)      :: g_max

    integer :: err

    ! Global maximum and local maximum are the same thing in a non-mpi build
    g_max = l_max
    err=0 ! Set local variable to avoid unused variable errors

  end subroutine global_max_real64


  !> Calculates the global maximum of a collection of local real32 maximums
  !>
  !> @param l_min The max on the local partition
  !> @param g_max The calculated global maximum
  !>
  subroutine global_max_real32(self, l_max, g_max)
    implicit none
    class(mpi_type), intent(inout) :: self
    real(real32), intent(in)       :: l_max
    real(real32), intent(out)      :: g_max

    integer :: err

    ! Global maximum and local maximum are the same thing in a non-mpi build
    g_max = l_max
    err=0 ! Set local variable to avoid unused variable errors

  end subroutine global_max_real32


  !> Calculates the global maximum of a collection of local integer maximums
  !>
  !> @param l_min The max on the local partition
  !> @param g_max The calculated global maximum
  !>
  subroutine global_max_int32(self, l_max, g_max)
    implicit none
    class(mpi_type), intent(inout) :: self
    integer(int32), intent(in)     :: l_max
    integer(int32), intent(out)    :: g_max

    integer :: err

    ! Global maximum and local maximum are the same thing in a non-mpi build
    g_max = l_max
    err=0 ! Set local variable to avoid unused variable errors

  end subroutine global_max_int32


  !> Gather integer data from all MPI tasks into a single array in all MPI tasks
  !> The data in send_buffer from the jth process is received by every
  !> process and placed in the jth block of the buffer recv_buffer.
  !>
  !> @param send_buffer The buffer of data to be sent to all MPI tasks
  !> @param recv_buffer The buffer into which the gathered data will be placed
  !> @param count The number of items in send_buffer
  subroutine all_gather(self, send_buffer, recv_buffer, count)
    implicit none
    class(mpi_type), intent(inout) :: self
    integer(int32), intent(in)     :: send_buffer(:)
    integer(int32), intent(out)    :: recv_buffer(:)
    integer(int32), intent(in)     :: count

    integer :: err

    ! Send and recv buffers in a gather are the same thing in a non-mpi build
    recv_buffer = send_buffer
    err=0 ! Set local variable to avoid unused variable errors
  end subroutine all_gather


  !> Broadcasts 1d array of logical data from the root MPI task to all other
  !> MPI tasks
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_logical_1d(self, buffer, count, root)

    implicit none

    class(mpi_type), intent(inout) :: self
    logical,         intent(inout) :: buffer(:)
    integer,         intent(in)    :: count
    integer,         intent(in)    :: root

    integer(int32) :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing
    err=0 ! Set local variable to avoid unused variable errors
  end subroutine broadcast_logical_1d

  !> Broadcasts 1d array of 32-bit integer data from the root MPI task to all
  !> other MPI tasks
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_int32_1d(self, buffer, count, root)

    implicit none

    class(mpi_type), intent(inout) :: self
    integer(int32),  intent(inout) :: buffer(:)
    integer,         intent(in)    :: count
    integer,         intent(in)    :: root

    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing
    err=0 ! Set local variable to avoid unused variable errors
  end subroutine broadcast_int32_1d

  !> Broadcasts 1d array of 64-bit real data from the root MPI task to all
  !> other MPI tasks.
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_real64_1d(self, buffer, count, root)

    implicit none

    class(mpi_type), intent(inout) :: self
    real(real64),    intent(inout) :: buffer(:)
    integer,         intent(in)    :: count
    integer,         intent(in)    :: root

    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing
    err=0 ! Set local variable to avoid unused variable errors
  end subroutine broadcast_real64_1d

  !> Broadcasts 1d array of 32-bit real data from the root MPI task to all
  !> other MPI tasks.
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_real32_1d(self, buffer, count, root)

    implicit none

    class(mpi_type), intent(inout) :: self
    real(real32),    intent(inout) :: buffer(:)
    integer,         intent(in)    :: count
    integer,         intent(in)    :: root

    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing
    err=0 ! Set local variable to avoid unused variable errors
  end subroutine broadcast_real32_1d

  !> Broadcasts 1d array of character data from the root MPI task to all other
  !> MPI tasks
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_str_1d(self, buffer, count, root)

    implicit none

    class(mpi_type),  intent(inout) :: self
    character(len=*), intent(inout) :: buffer(:)
    integer,          intent(in)    :: count
    integer,          intent(in)    :: root

    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing
    err=0 ! Set local variable to avoid unused variable errors
  end subroutine broadcast_str_1d

  !> Broadcasts 2d array of logical data from the root MPI task to all other
  !> MPI tasks
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_logical_2d(self, buffer, count, root)

    implicit none

    class(mpi_type), intent(inout) :: self
    logical,         intent(inout) :: buffer(:,:)
    integer,         intent(in)    :: count
    integer,         intent(in)    :: root

    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing
    err=0 ! Set local variable to avoid unused variable errors
  end subroutine broadcast_logical_2d

  !> Broadcasts 2d array of 32-bit integer data from the root MPI task to all
  !> other MPI tasks
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_int32_2d(self, buffer, count, root)

    implicit none

    class(mpi_type), intent(inout) :: self
    integer(int32),  intent(inout) :: buffer(:,:)
    integer, intent(in)            :: count
    integer, intent(in)            :: root

    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing
    err=0 ! Set local variable to avoid unused variable errors
  end subroutine broadcast_int32_2d

  !> Broadcasts 2d array of 64-bit real data from the root MPI task to all
  !> other MPI tasks.
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_real64_2d(self, buffer, count, root)

    implicit none

    class(mpi_type), intent(inout) :: self
    real(real64),    intent(inout) :: buffer(:,:)
    integer, intent(in)            :: count
    integer, intent(in)            :: root

    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing
    err=0 ! Set local variable to avoid unused variable errors
  end subroutine broadcast_real64_2d

  !> Broadcasts 2d array of 32-bit real data from the root MPI task to all
  !> other MPI tasks.
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_real32_2d(self, buffer, count, root)

    implicit none

    class(mpi_type), intent(inout) :: self
    real(real32),    intent(inout) :: buffer(:,:)
    integer,         intent(in)    :: count
    integer,         intent(in)    :: root

    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing
    err=0 ! Set local variable to avoid unused variable errors
  end subroutine broadcast_real32_2d

  !> Broadcasts 3d array of logical data from the root MPI task to all other
  !> MPI tasks
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_logical_3d(self, buffer, count, root)

    implicit none

    class(mpi_type), intent(inout) :: self
    logical,         intent(inout) :: buffer(:,:,:)
    integer,         intent(in)    :: count
    integer,         intent(in)    :: root

    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing
    err=0 ! Set local variable to avoid unused variable errors
  end subroutine broadcast_logical_3d

  !> Broadcasts 3d array of 32-bit integer data from the root MPI task to all
  !> other MPI tasks
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_int32_3d(self, buffer, count, root)

    implicit none

    class(mpi_type), intent(inout) :: self
    integer(int32),  intent(inout) :: buffer(:,:,:)
    integer,         intent(in)    :: count
    integer,         intent(in)    :: root

    integer(int32) :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing
    err=0 ! Set local variable to avoid unused variable errors
  end subroutine broadcast_int32_3d

  !> Broadcasts 3d array of 64-bit real data from the root MPI task to all
  !> other MPI tasks.
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_real64_3d(self, buffer, count, root)

    implicit none

    class(mpi_type), intent(inout) :: self
    real(real64),    intent(inout) :: buffer(:,:,:)
    integer,         intent(in)    :: count
    integer,         intent(in)    :: root

    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing
    err=0 ! Set local variable to avoid unused variable errors
  end subroutine broadcast_real64_3d

  !> Broadcasts 3d array of 32-bit real data from the root MPI task to all
  !> other MPI tasks.
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_real32_3d(self, buffer, count, root)

    implicit none

    class(mpi_type), intent(inout) :: self
    real(real32),    intent(inout) :: buffer(:,:,:)
    integer,         intent(in)    :: count
    integer,         intent(in)    :: root

    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing
    err=0 ! Set local variable to avoid unused variable errors
  end subroutine broadcast_real32_3d



  !> Returns the number of MPI ranks in the communicator
  !>
  !> @return c_size The number of MPI ranks in the communicator
  function get_comm_size(self) result(c_size)
    implicit none
    class(mpi_type), intent(inout)  :: self
    integer :: c_size
    ! A non-mpi run is serial, therefore, number of ranks has to be one
    c_size = 1
  end function get_comm_size

  !> Returns the number of the local MPI rank
  !>
  !> @return c_size The number of the local MPI rank
  function get_comm_rank(self) result(c_rank)
    implicit none
    class(mpi_type), intent(inout)  :: self
    integer :: c_rank
    ! A non-mpi run is serial, therefore, local rank is always rank zero
    c_rank = 0
  end function get_comm_rank

end module mpi_mod
