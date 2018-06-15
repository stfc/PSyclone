!> Module containing state and utilities for managing OpenCL device
module ocl_env_mod
  use iso_c_binding, only: c_intptr_t
  use ocl_utils_mod, only: CL_UTIL_STR_LEN, init_device
  use ocl_params_mod
  implicit none

  private

  !> Whether or not this module has been initialised
  logical, save :: cl_env_initialised = .False.
  !> Pointer to the OpenCL device being used
  integer(c_intptr_t) :: cl_device
  !> The OpenCL context
  integer(c_intptr_t) :: cl_context
  !> Version of OpenCL supported by the device
  character(len=CL_UTIL_STR_LEN) :: cl_version_str

  !> Number of OpenCL command queues
  integer, save :: cl_num_queues
  !> Array of command queues - used to achieve concurrent execution
  integer(c_intptr_t), allocatable, target :: cl_cmd_queues(:)

  !> The OpenCL kernels used by the model
  integer, save :: cl_num_kernels
  !> The maximum number of kernels we can store
  integer, parameter :: cl_max_num_kernels = 30
  character(len=CL_UTIL_STR_LEN) :: cl_kernel_names(cl_max_num_kernels)
  integer(c_intptr_t), target :: cl_kernels(cl_max_num_kernels)

  public ocl_env_init
  public cl_context, cl_device, get_num_cmd_queues, get_cmd_queues

contains

  !===================================================

  !> Initialise the GOcean environment
  subroutine ocl_env_init()
    use ocl_utils_mod, only: init_device
    implicit none
    integer :: ierr

    if(cl_env_initialised)return

    ! Initialise the OpenCL device
    call init_device(cl_device, cl_version_str, cl_context)

    ! Create command queue(s)
    cl_num_queues = 4
    allocate(cl_cmd_queues(cl_num_queues), Stat=ierr)
    if(ierr /= 0)then
       stop "Failed to allocate list for OpenCL command queues"
    end if
    call init_cmd_queues(cl_num_queues, cl_cmd_queues, cl_context, cl_device)

    ! At this point we have no kernels
    cl_num_kernels = 0

    ! Environment now initialised
    cl_env_initialised = .True.

  end subroutine ocl_env_init

  !===================================================

  subroutine add_kernels(nkernels, kernel_names, filename)
    use iso_c_binding, only: c_intptr_t
    use ocl_utils_mod, only: get_program, get_kernel, release_program
    integer, intent(in) :: nkernels
    character(len=*), intent(in) :: kernel_names(nkernels)
    character(len=*), intent(in), optional :: filename
    ! Locals
    integer :: ik, ierr, new_kern_count
    integer(c_intptr_t), target :: prog
    character(len=300) :: lfilename

    if(.not. cl_env_initialised)then
       call ocl_env_init()
    end if

    if(.not. present(filename))then
       call get_environment_variable("PSYCLONE_KERNELS_FILE", lfilename)
    else
       lfilename = filename
    end if

    if((nkernels + cl_num_kernels) > cl_max_num_kernels)then
       write(*,"('add_kernels: Adding ',I2,' kernels will exceed the &
&maximum number of ',I3,' - increase ocl_env_mod::cl_max_num_kernels')") &
          nkernels, cl_max_num_kernels
       stop
    end if

    ! Get a program object containing all of our kernels
    prog = get_program(cl_context, cl_device, cl_version_str, lfilename)

    new_kern_count = 0
    do ik = 1, nkernels
       ! Skip any kernels we've already got
       if(get_kernel_index(kernel_names(ik)) /= 0)cycle
       new_kern_count = new_kern_count + 1
       cl_kernels(cl_num_kernels+new_kern_count) = get_kernel(prog, &
                                                              kernel_names(ik))
    end do
    cl_num_kernels = cl_num_kernels + new_kern_count

    ! Release the program now that we've created the kernels
    call release_program(prog)

  end subroutine add_kernels

  !===================================================

  function get_kernel_by_name(name) result(kern)
    integer(c_intptr_t), target :: kern
    character(len=*), intent(in) :: name
    ! Locals
    integer :: ik, match
    character(len=256) :: msg

    match = get_kernel_index(name)

    if(match == 0)then
       !> \TODO add check that we don't go out of bounds when writing to msg
       write(*, "('get_kernel_by_name: no kernel with name ',(A),' found')")&
            name
       stop
    end if

    kern = cl_kernels(match)

  end function get_kernel_by_name

  !===================================================

  function get_kernel_index(name) result index
    integer :: index
    character(len=*), intent(in) :: name
    integer :: ik
    !> Helper routine to search for a kernel by name. Returns the
    !! index of the kernel (in the cl_kernels list) if found or 0.
    index = 0
    !> \TODO is there a better way to do this that reduces the need for
    !! string comparisons?
    do ik = 1, cl_num_kernels
       if(name == cl_kernel_names(ik))then
          ! We can't just return out of this loop because this is a
          ! function
          index = ik
          exit
       end if
    end do
  end function get_kernel_index

  !===================================================

  function get_num_cmd_queues() result(num)
    integer :: num
    num = cl_num_queues
  end function get_num_cmd_queues

  !===================================================

  function get_cmd_queues() result(queues)
    integer(c_intptr_t), pointer :: queues(:)
    queues => cl_cmd_queues
  end function get_cmd_queues

  !===================================================

  subroutine ocl_release()
    integer :: i

    do i=1, cl_num_kernels
       call release_kernel(cl_kernels(i))
    end do

    call release_queues(cl_num_queues, cl_cmd_queues)

    call release_context(cl_context)

  end subroutine ocl_release

end module ocl_env_mod
