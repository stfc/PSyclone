










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
module lfric_mpi_mod

  use, intrinsic :: iso_fortran_env, only : int32, real32, real64

  use constants_mod, only : str_def, real_type, integer_type, logical_type
  ! No "use mpi" in non-mpi build
  use log_mod,       only : log_event, LOG_LEVEL_ERROR

  implicit none

  private

  public global_mpi, create_comm, destroy_comm, get_lfric_datatype

  ! When running without MPI, create a dummy object that looks like an MPI_F08
  ! communicator object
  type, public :: mpi_comm
    integer, public :: mpi_val
  end type

  type, public :: lfric_mpi_type
    private
    !> The mpi communicator
    type(mpi_comm) :: comm
    integer :: comm_size
    integer :: comm_rank
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
    procedure, public :: broadcast_logical_scalar
    procedure, public :: broadcast_int32_scalar
    procedure, public :: broadcast_real64_scalar
    procedure, public :: broadcast_real32_scalar
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
    generic :: broadcast => broadcast_logical_scalar, &
                            broadcast_int32_scalar,   &
                            broadcast_real64_scalar,  &
                            broadcast_real32_scalar,  &
                            broadcast_logical_1d,     &
                            broadcast_int32_1d,       &
                            broadcast_real64_1d,      &
                            broadcast_real32_1d,      &
                            broadcast_str_1d,         &
                            broadcast_logical_2d,     &
                            broadcast_int32_2d,       &
                            broadcast_real64_2d,      &
                            broadcast_real32_2d,      &
                            broadcast_logical_3d,     &
                            broadcast_int32_3d,       &
                            broadcast_real64_3d,      &
                            broadcast_real32_3d
    procedure, public :: get_comm_size
    procedure, public :: get_comm_rank

  end type lfric_mpi_type

  ! Define an "LFRic" type that we can pass around that holds a communicator
  type, public :: lfric_comm_type
    private
    type(mpi_comm) :: comm
  contains
    procedure, public :: get_comm_mpi_val
    procedure, public :: set_comm_mpi_val
  end type

  ! When running without MPI, create a dummy object that looks like an MPI_F08
  ! datatype object
  type, public :: mpi_datatype
    integer, public :: mpi_val
  end type

  ! Define an "LFRic" type that we can pass around that holds an mpi datatype
  type, public :: lfric_datatype_type
    private
    type(mpi_datatype) :: datatype
  contains
    procedure, public :: get_datatype_mpi_val
    procedure, public :: get_mpi_datatype
  end type

  !Global MPI object
  !> @todo This needs to be moved out of global scope and into the modeldb
  !>       object, when that object exists
  type(lfric_mpi_type), target :: global_mpi

contains

  !> Helper function (not type bound) that creates a communticator
  !> by initialsing mpi and returning mpi_comm_world
  !>
  !> @param out_comm An MPI communicator.
  !>
  subroutine create_comm(out_comm)
    implicit none
    type(lfric_comm_type), intent(out) :: out_comm
    integer :: ierr

    ! Don't initialise mpi in non-mpi build.
    out_comm%comm%mpi_val = 0
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
  function get_lfric_datatype( fortran_type, fortran_kind ) result(mpi_datatype)
    use, intrinsic :: iso_fortran_env, only : real128, real64, real32, &
                                              int64, int32, int16, int8
    implicit none
    integer,       intent(in) :: fortran_type
    integer,       intent(in) :: fortran_kind
    type(lfric_datatype_type) :: mpi_datatype

    ! In a non-mpi build the mpi datatype is meaningless - just return zero
    mpi_datatype%datatype%mpi_val = 0

  end function get_lfric_datatype

  !> Stores the MPI communicator in a private variable, ready for later use.
  !>
  !> @param in_comm The MPI communicator to be stored.
  !>
  subroutine initialise(self, in_comm)
    implicit none
    class(lfric_mpi_type), intent(inout) :: self
    type(lfric_comm_type), intent(in)    :: in_comm
    integer :: ierr

    ! Store incoming dummy communicator
    self%comm = in_comm%comm
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
    class(lfric_mpi_type), intent(inout) :: self
    self%comm_set = .false.
  end subroutine finalise

  !> Returns the stored MPI communicator
  !> @return communicator The stored MPI communicator
  !>
  function get_comm(self) result(communicator)
    implicit none
    class(lfric_mpi_type), intent(in) :: self
    type(lfric_comm_type) :: communicator
    communicator%comm = self%comm
  end function get_comm

  !> Returns whether the MPI communicator has been stored
  !> @return comm_state A flag indicating whether the MPI communicator is stored
  !>
  function is_comm_set(self) result(comm_state)
    implicit none
    class(lfric_mpi_type), intent(inout) :: self
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
    class(lfric_mpi_type), intent(inout) :: self
    real(real64),          intent(in)    :: l_sum
    real(real64),          intent(out)   :: g_sum

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! Global sum and local sum are the same thing in a non-mpi build
    g_sum = l_sum

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0

  end subroutine global_sum_real64


  !> Calculates the global sum of a collection of real32 local sums
  !>
  !> @param l_sum The sum of the reals on the local partition
  !> @param g_sum The calculated global sum
  !>
  subroutine global_sum_real32(self, l_sum, g_sum)
    implicit none
    class(lfric_mpi_type), intent(inout) :: self
    real(real32),          intent(in)    :: l_sum
    real(real32),          intent(out)   :: g_sum

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! Global sum and local sum are the same thing in a non-mpi build
    g_sum = l_sum

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0

  end subroutine global_sum_real32


  !> Calculates the global sum of a collection of integer local sums
  !>
  !> @param l_sum The sum of the integers on the local partition
  !> @param g_sum The calculated global sum
  !>
  subroutine global_sum_int32(self, l_sum, g_sum)
    implicit none
    class(lfric_mpi_type), intent(inout) :: self
    integer(int32),        intent(in)    :: l_sum
    integer(int32),        intent(out)   :: g_sum

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! Global sum and local sum are the same thing in a non-mpi build
    g_sum = l_sum

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0

  end subroutine global_sum_int32


  !> Calculates the global minimum of a collection of local real minimums
  !>
  !> @param l_min The min on the local partition
  !> @param g_min The calculated global minimum
  !>
  subroutine global_min_real64(self, l_min, g_min)
    implicit none
    class(lfric_mpi_type), intent(inout) :: self
    real(real64),          intent(in)    :: l_min
    real(real64),          intent(out)   :: g_min

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! Global minimum and local minimum are the same thing in a non-mpi build
    g_min = l_min

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0

  end subroutine global_min_real64

  !> Calculates the global minimum of a collection of local real32 minimums
  !>
  !> @param l_min The min on the local partition
  !> @param g_min The calculated global minimum
  !>
  subroutine global_min_real32(self, l_min, g_min)
    implicit none
    class(lfric_mpi_type), intent(inout) :: self
    real(real32),          intent(in)    :: l_min
    real(real32),          intent(out)   :: g_min

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! Global minimum and local minimum are the same thing in a non-mpi build
    g_min = l_min

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0

  end subroutine global_min_real32


  !> Calculates the global minimum of a collection of local integer minimums
  !>
  !> @param l_min The min on the local partition
  !> @param g_min The calculated global minimum
  !>
  subroutine global_min_int32(self, l_min, g_min)
    implicit none
    class(lfric_mpi_type), intent(inout) :: self
    integer(int32),        intent(in)    :: l_min
    integer(int32),        intent(out)   :: g_min

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! Global minimum and local minimum are the same thing in a non-mpi build
    g_min = l_min

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0

  end subroutine global_min_int32


  !> Calculates the global maximum of a collection of local real maximums
  !>
  !> @param l_min The max on the local partition
  !> @param g_max The calculated global maximum
  !>
  subroutine global_max_real64(self, l_max, g_max)
    implicit none
    class(lfric_mpi_type), intent(inout) :: self
    real(real64),          intent(in)    :: l_max
    real(real64),          intent(out)   :: g_max

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! Global maximum and local maximum are the same thing in a non-mpi build
    g_max = l_max

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0

  end subroutine global_max_real64


  !> Calculates the global maximum of a collection of local real32 maximums
  !>
  !> @param l_min The max on the local partition
  !> @param g_max The calculated global maximum
  !>
  subroutine global_max_real32(self, l_max, g_max)
    implicit none
    class(lfric_mpi_type), intent(inout) :: self
    real(real32),          intent(in)    :: l_max
    real(real32),          intent(out)   :: g_max

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! Global maximum and local maximum are the same thing in a non-mpi build
    g_max = l_max

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0

  end subroutine global_max_real32


  !> Calculates the global maximum of a collection of local integer maximums
  !>
  !> @param l_min The max on the local partition
  !> @param g_max The calculated global maximum
  !>
  subroutine global_max_int32(self, l_max, g_max)
    implicit none
    class(lfric_mpi_type), intent(inout) :: self
    integer(int32),        intent(in)    :: l_max
    integer(int32),        intent(out)   :: g_max

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! Global maximum and local maximum are the same thing in a non-mpi build
    g_max = l_max

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0

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
    class(lfric_mpi_type), intent(inout) :: self
    integer(int32),        intent(in)    :: send_buffer(:)
    integer(int32),        intent(out)   :: recv_buffer(:)
    integer(int32),        intent(in)    :: count

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! Send and recv buffers in a gather are the same thing in a non-mpi build
    recv_buffer = send_buffer
    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0
  end subroutine all_gather

  !> Broadcasts a logical scalar from the root MPI task to all other
  !> MPI tasks
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be written to here
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_logical_scalar(self, buffer, root)

    implicit none

    class(lfric_mpi_type), intent(inout) :: self
    logical,               intent(inout) :: buffer
    integer,               intent(in)    :: root

    integer(int32) :: err
    logical :: buffer_array(1)

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    err=0
    buffer_array(1)=.false.
  end subroutine broadcast_logical_scalar

  !> Broadcasts a 32-bit integer scalar from the root MPI task to all
  !> other MPI tasks
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be written to here
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_int32_scalar(self, buffer, root)

    implicit none

    class(lfric_mpi_type), intent(inout) :: self
    integer(int32),        intent(inout) :: buffer
    integer,               intent(in)    :: root

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err
    integer(int32) :: buffer_array(1)

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0
    buffer_array(1)=0
  end subroutine broadcast_int32_scalar

  !> Broadcasts a 64-bit real data scalar from the root MPI task to all
  !> other MPI tasks.
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be written to here
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_real64_scalar(self, buffer, root)

    implicit none

    class(lfric_mpi_type), intent(inout) :: self
    real(real64),          intent(inout) :: buffer
    integer,               intent(in)    :: root

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err
    real(real64) :: buffer_array(1)

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0
    buffer_array(1)=0.0_real64
  end subroutine broadcast_real64_scalar

  !> Broadcasts a 32-bit real data scalar from the root MPI task to all
  !> other MPI tasks.
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be written to here
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_real32_scalar(self, buffer, root)

    implicit none

    class(lfric_mpi_type), intent(inout) :: self
    real(real32),          intent(inout) :: buffer
    integer,               intent(in)    :: root

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err
    real(real32) :: buffer_array(1)

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0
    buffer_array(1)=0.0_real32
  end subroutine broadcast_real32_scalar

  !> Broadcasts 1d array of logical data from the root MPI task to all other
  !> MPI tasks
  !>
  !> @param buffer On the root MPI task, contains the data to broadcast,
  !>               on other tasks the data from root task will be writen to here
  !> @param count The number of items in buffer
  !> @param root The MPI task from which data will be broadcast
  subroutine broadcast_logical_1d(self, buffer, count, root)

    implicit none

    class(lfric_mpi_type), intent(inout) :: self
    logical,               intent(inout) :: buffer(:)
    integer,               intent(in)    :: count
    integer,               intent(in)    :: root

    integer(int32) :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    err=0
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

    class(lfric_mpi_type), intent(inout) :: self
    integer(int32),        intent(inout) :: buffer(:)
    integer,               intent(in)    :: count
    integer,               intent(in)    :: root

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0
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

    class(lfric_mpi_type), intent(inout) :: self
    real(real64),          intent(inout) :: buffer(:)
    integer,               intent(in)    :: count
    integer,               intent(in)    :: root

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0
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

    class(lfric_mpi_type), intent(inout) :: self
    real(real32),          intent(inout) :: buffer(:)
    integer,               intent(in)    :: count
    integer,               intent(in)    :: root

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0
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

    class(lfric_mpi_type),  intent(inout) :: self
    character(len=*),       intent(inout) :: buffer(:)
    integer,                intent(in)    :: count
    integer,                intent(in)    :: root

    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    err=0
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

    class(lfric_mpi_type), intent(inout) :: self
    logical,               intent(inout) :: buffer(:,:)
    integer,               intent(in)    :: count
    integer,               intent(in)    :: root

    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    err=0
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

    class(lfric_mpi_type), intent(inout) :: self
    integer(int32),        intent(inout) :: buffer(:,:)
    integer,               intent(in)    :: count
    integer,               intent(in)    :: root

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0
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

    class(lfric_mpi_type), intent(inout) :: self
    real(real64),          intent(inout) :: buffer(:,:)
    integer,               intent(in)    :: count
    integer,               intent(in)    :: root

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0
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

    class(lfric_mpi_type), intent(inout) :: self
    real(real32),          intent(inout) :: buffer(:,:)
    integer,               intent(in)    :: count
    integer,               intent(in)    :: root

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0
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

    class(lfric_mpi_type), intent(inout) :: self
    logical,               intent(inout) :: buffer(:,:,:)
    integer,               intent(in)    :: count
    integer,               intent(in)    :: root

    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    err=0
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

    class(lfric_mpi_type), intent(inout) :: self
    integer(int32),        intent(inout) :: buffer(:,:,:)
    integer,               intent(in)    :: count
    integer,               intent(in)    :: root

    type(lfric_datatype_type) :: lfric_datatype
    integer(int32) :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0
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

    class(lfric_mpi_type), intent(inout) :: self
    real(real64),          intent(inout) :: buffer(:,:,:)
    integer,               intent(in)    :: count
    integer,               intent(in)    :: root

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0
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

    class(lfric_mpi_type), intent(inout) :: self
    real(real32),          intent(inout) :: buffer(:,:,:)
    integer,               intent(in)    :: count
    integer,               intent(in)    :: root

    type(lfric_datatype_type) :: lfric_datatype
    integer :: err

    ! In a non-mpi build there is nowhere to broadcast to - so do nothing

    ! Set local variables to avoid unused variable errors
    lfric_datatype%datatype%mpi_val = 0
    err=0
  end subroutine broadcast_real32_3d



  !> Returns the number of MPI ranks in the communicator
  !>
  !> @return c_size The number of MPI ranks in the communicator
  function get_comm_size(self) result(c_size)
    implicit none
    class(lfric_mpi_type), intent(inout)  :: self
    integer :: c_size
    ! A non-mpi run is serial, therefore, number of ranks has to be one
    c_size = 1
  end function get_comm_size

  !> Returns the number of the local MPI rank
  !>
  !> @return c_size The number of the local MPI rank
  function get_comm_rank(self) result(c_rank)
    implicit none
    class(lfric_mpi_type), intent(inout)  :: self
    integer :: c_rank
    ! A non-mpi run is serial, therefore, local rank is always rank zero
    c_rank = 0
  end function get_comm_rank

  !> Returns the integer communicator
  !>
  !> @return comm The integer component of the communicator
  function get_comm_mpi_val(self) result(comm)
    implicit none
    class(lfric_comm_type), intent(in)  :: self
    integer :: comm
    comm = self%comm%mpi_val
  end function get_comm_mpi_val

  !> Sets the LFRic communicator to point at the given integer communicator
  !>
  !> @param comm Integer communicator that the LFRic comm should point at
  subroutine set_comm_mpi_val(self, comm)
    implicit none
    class(lfric_comm_type), intent(inout)  :: self
    integer, intent(in) :: comm
    self%comm%mpi_val = comm
  end subroutine set_comm_mpi_val

  !> Returns the integer datatype
  !>
  !> @return datatype The integer component of the datatype
  function get_datatype_mpi_val(self) result(datatype)
    implicit none
    class(lfric_datatype_type), intent(in)  :: self
    integer :: datatype
    datatype = self%datatype%mpi_val
  end function get_datatype_mpi_val

  !> Returns the mpi datatype
  !>
  !> @return datatype The integer component of the datatype
  function get_mpi_datatype(self) result(datatype)
    implicit none
    class(lfric_datatype_type), intent(inout)  :: self
    type(mpi_datatype) :: datatype
    datatype = self%datatype
  end function get_mpi_datatype

end module lfric_mpi_mod
