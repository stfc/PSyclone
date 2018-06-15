module ocl_utils_mod
  use clfortran
  use iso_c_binding
  implicit none

  integer, parameter :: CL_UTIL_STR_LEN = 64

contains
  
  subroutine init_device(device, version_str, context)
    !> Initialise an OpenCL device
    integer(c_intptr_t), intent(inout) :: device, context 
    character(len=CL_UTIL_STR_LEN), intent(inout) :: version_str
    ! Locals
    integer :: iplatform, idevice, iallocerr
    integer(c_intptr_t), target :: ctx_props(3)
    integer(c_int32_t), target :: device_cu
    integer(c_size_t) :: iret, zero_size = 0
    integer(c_int32_t) :: ierr, num_devices, num_platforms
    integer(c_intptr_t), allocatable, target :: &
       platform_ids(:), device_ids(:)
    character(len=1,kind=c_char), allocatable, target :: device_name(:)

    ierr = clGetPlatformIDs(0, C_NULL_PTR, num_platforms)
    call check_status('clGetPlatformIDs', ierr)
    if (num_platforms < 1)then
       write (*,*) "Failed to get any OpenCL platform IDs"
       stop
    end if
    print '(a,i2)','Num Platforms: ',num_platforms

    allocate(platform_ids(num_platforms), stat=iallocerr)
    if (iallocerr.ne.0) stop 'memory allocation error'

    ! whenever "&" appears in C subroutine (address-of) call,
    ! then C_LOC has to be used in Fortran
    ierr = clGetPlatformIDs(num_platforms, C_LOC(platform_ids), &
                            num_platforms)
    call check_status('clGetPlatformIDs', ierr)

    ! Get device IDs only for platform 1
    iplatform=1

    ierr=clGetDeviceIDs(platform_ids(iplatform), CL_DEVICE_TYPE_ALL, &
                        0, C_NULL_PTR, num_devices)
    call check_status('clGetDeviceIDs', ierr)
    if (num_devices < 1)then
       stop 'Failed to find any OpenCL devices'
    end if
    print '(a,i2)','Num Devices: ',num_devices

    allocate(device_ids(num_devices), stat=iallocerr)
    if (iallocerr.ne.0) stop 'memory allocation error'

    ! whenever "&" appears in C subroutine (address-off) call,
    ! then C_LOC has to be used in Fortran
    ierr = clGetDeviceIDs(platform_ids(iplatform), CL_DEVICE_TYPE_ALL, &
                          num_devices, C_LOC(device_ids), num_devices)
    call check_status('clGetDeviceIDs', ierr)

    ! Get device info only for device 1
    idevice=1
    device = device_ids(idevice)

    ierr=clGetDeviceInfo(device_ids(idevice), &
                         CL_DEVICE_MAX_COMPUTE_UNITS, sizeof(device_cu), &
                         C_LOC(device_cu), iret)
    call check_status('clGetDeviceInfo', ierr)
    ierr=clGetDeviceInfo(device_ids(idevice), &
                         CL_DEVICE_NAME, zero_size, C_NULL_PTR,iret)
    call check_status('clGetDeviceInfo', ierr)

    allocate(device_name(iret), stat=iallocerr)
    if (iallocerr.ne.0) stop 'allocate'

    ierr=clGetDeviceInfo(device_ids(idevice), CL_DEVICE_NAME, &
                         sizeof(device_name), C_LOC(device_name), iret)
    if (ierr.ne.CL_SUCCESS) stop 'clGetDeviceInfo'

    write (*,'(a,i2,a,i3,a)',advance='no') &
         ' Device (#',idevice,', Compute Units: ',device_cu,') - '
    print *,device_name(1:iret)
    deallocate(device_name)

    print '(a,i2,a)', 'Creating context for: ', num_devices,' devices'
    print '(a,i2)', 'for platform: ',iplatform
    ctx_props(1) = CL_CONTEXT_PLATFORM
    ctx_props(2) = platform_ids(iplatform)
    ctx_props(3) = 0
    context = clCreateContext(C_LOC(ctx_props), num_devices, &
                              C_LOC(device_ids), C_NULL_FUNPTR, C_NULL_PTR, &
                              ierr)
    call check_status('clCreateContext', ierr)

  end subroutine init_device

  !===================================================

  subroutine release_context(context)
    integer(c_intptr_t), intent(inout) :: context 
    ! Locals
    integer(c_int32_t) :: ierr

    ierr=clReleaseContext(context)
    call check_status('clReleaseContext', ierr)

  end subroutine release_context

  !===================================================

  function get_program(context, device, version_str, filename) result(prog)
    integer(c_intptr_t), target :: prog
    integer(c_intptr_t), intent(inout), target :: device, context 
    character(len=CL_UTIL_STR_LEN), intent(in) :: version_str, filename
    ! Locals
    character(len=1,kind=c_char), allocatable, target :: source(:)
    type(c_ptr), target :: psource
    character(len=1024) :: options
    character(len=1, kind=c_char), target :: retinfo(1:1024), c_options(1:1024)
    integer :: i, irec, iallocerr
    integer(c_int32_t) :: ierr
    integer, parameter :: iunit=10
    integer(c_size_t), target :: binary_size, iret
    character, dimension(1) :: char

    ! read kernel from disk
    open(iunit, file=filename, access='direct', &
         status='old', action='read', iostat=ierr, recl=1)
    if (ierr.ne.0)then
       write(*,*) 'Cannot open file: ', TRIM(filename)
       stop
    end if
    irec=1
    do
       read(iunit, rec=irec, iostat=ierr) char
       if (ierr /= 0) exit
       irec = irec+1
    end do

    if (irec.eq.0) stop 'nothing read'
    allocate(source(irec+1),stat=iallocerr)
    if (iallocerr.ne.0) stop 'allocate'
    do i=1,irec
       read(iunit,rec=i,iostat=ierr) source(i:i)
    enddo
    close(iunit)

    print '(a,i7)','size of source code in bytes: ',irec

    psource=C_LOC(source) ! pointer to source code
    binary_size = irec
    prog = clCreateProgramWithBinary(context, 1, C_LOC(device), &
                                     C_LOC(binary_size), C_LOC(psource), &
                                     C_NULL_PTR, ierr)

    call check_status('clCreateProgramWithSource', ierr)

    options = "" !'-cl-opt-disable' ! compiler options
    irec = len(trim(options))
    do i=1, irec
       c_options(i)=options(i:i)
    enddo
    c_options(irec+1) = C_NULL_CHAR
    ierr=clBuildProgram(prog, 0, C_NULL_PTR, C_LOC(c_options), &
         C_NULL_FUNPTR,C_NULL_PTR)
    if (ierr.ne.CL_SUCCESS) then
       print *,'clBuildProgram',ierr
       ierr=clGetProgramBuildInfo(prog, device, CL_PROGRAM_BUILD_LOG, &
            sizeof(retinfo), C_LOC(retinfo),iret)
       if (ierr.ne.0) stop 'clGetProgramBuildInfo'
       print '(a)','build log start'
       print '(1024a)',retinfo(1:min(iret,1024))
       print '(a)','build log end'
       stop
    endif

  end function get_program


  !> Release an OpenCL program object
  subroutine release_program(prog)
    integer(c_intptr_t), target, intent(inout) :: prog
    ! Locals
    integer(c_int32_t) :: ierr

    ierr = clReleaseProgram(prog)
    call check_status('clReleaseProgram', ierr)
  end subroutine release_program


  !> Get a kernel object from a program object
  !>
  !> @param [in] prog OpenCL program object obtained from
  !>                  <code>get_program</code>
  !> @return the kernel object
  !>
  function get_kernel(prog, kernel_name) result(kernel)
    integer(c_intptr_t), target, intent(in) :: prog
    character(len=*), intent(in) :: kernel_name
    integer(c_intptr_t), target :: kernel
    ! Locals
    integer :: irec, i
    character(len=1, kind=c_char), target :: c_kernel_name(1:1024)

    irec = len(trim(kernel_name))
    do i=1, irec
       c_kernel_name(i) = kernel_name(i:i)
    enddo
    c_kernel_name(irec+1) = C_NULL_CHAR

    kernel = clCreateKernel(prog, C_LOC(c_kernel_name), irec)
    call check_status('clCreateKernel', irec)

  end function get_kernel

  !===================================================

  subroutine release_kernel(kern)
    integer(c_intptr_t), target, intent(inout) :: kern
    integer(c_int32_t) :: ierr

    ierr = clReleaseKernel(kern)
    call check_status('clReleaseKernel', ierr)

  end subroutine release_kernel

  !===================================================

  !> Set-up the specified number of OpenCL command queues for the specified
  !! context and device.
  subroutine init_cmd_queues(nqueues, queues, context, device)
    !> The number of command queues to create
    integer, intent(in) :: nqueues
    integer(c_intptr_t), target, intent(inout) :: queues(nqueues)
    integer(c_intptr_t), intent(in) :: device
    integer(c_intptr_t), intent(in) :: context
    ! Locals
    integer :: i
    integer(c_int32_t) :: ierr

    do i=1, nqueues
       queues(i) = clCreateCommandQueue(context, device, &
                                        CL_QUEUE_PROFILING_ENABLE, ierr)
       call check_status('clCreateCommandQueue', ierr)
    end do

  end subroutine init_cmd_queues

  !===================================================

  subroutine release_cmd_queues(nqueues, queues)
    integer, intent(in) :: nqueues
    integer(c_intptr_t), target, intent(inout) :: queues(nqueues)
    ! Locals
    integer :: iq
    integer(c_int32_t) :: ierr

    do iq=1, nqueues
       ierr=clReleaseCommandQueue(queues(iq))
       call check_status('clReleaseCommandQueue', ierr)
    end do

  end subroutine release_cmd_queues

  !===================================================

  !> Create a buffer in the supplied OpenCL context
  function create_buffer(context, access, nbytes) result(buffer)
    integer(c_intptr_t), intent(in) :: context
    integer(c_int64_t), intent(in) :: access
    integer(c_size_t), intent(in) :: nbytes
    integer(c_intptr_t), target :: buffer
    ! Locals
    integer(c_int32_t) :: ierr

    buffer = clCreateBuffer(context, access,          &
                            nbytes, C_NULL_PTR, ierr)
    call check_status('clCreateBuffer', ierr)

  end function create_buffer

  !=====================================================

  !> Read a buffer (containing 64-bit floats) from an OpenCL device. Call
  !! blocks until read is complete.
  subroutine read_buffer(queue, device_ptr, local_array, nelem)
    use ocl_params_mod, only: wp
    integer(c_intptr_t), intent(in) :: queue, device_ptr
    real(kind=wp), target, intent(in) :: local_array
    integer(8), intent(in) :: nelem
    ! Locals
    integer(c_int32_t) :: ierr
    integer(c_intptr_t), target :: event
    integer(8) :: nbytes

    nbytes = nelem * 8_8
    ierr = clEnqueueReadBuffer(queue, device_ptr, CL_TRUE, 0_8, &
                               nbytes, C_LOC(local_array),      &
                               0, C_NULL_PTR, C_LOC(event))
    call check_status('clEnqueueReadBuffer', ierr)

    !> \todo implement an asynchronous read so we don't have to wait
    !! for each one to complete.
    ierr = clWaitForEvents(1, C_LOC(event))
    call check_status('clWaitForEvents', ierr)

  end subroutine read_buffer

  !=====================================================

  !> Check the return code of an OpenCL API cal
  subroutine check_status(text, ierr)
    implicit none
    character(len=*), intent(in) :: text
    integer, intent(in) :: ierr

    logical, parameter :: verbose = .TRUE.

    if(ierr /= CL_SUCCESS)then
       write(*,'("Hit error: ",(A),": ",(A))') text, OCL_GetErrorString(ierr)
       stop
    end if
    if(verbose)then
       write(*,'("Called ",(A)," OK")') text 
    end if
  end subroutine check_status
  
function OCL_GetErrorString(error)
  implicit none
  character(len=64) :: OCL_GetErrorString
  integer, intent(in) :: error
  select case(error)

    case (CL_SUCCESS)
        OCL_GetErrorString = "CL_SUCCESS"
    case (CL_DEVICE_NOT_FOUND)
        OCL_GetErrorString = "CL_DEVICE_NOT_FOUND"
    case (CL_DEVICE_NOT_AVAILABLE)
        OCL_GetErrorString = "CL_DEVICE_NOT_AVAILABLE"
    case (CL_COMPILER_NOT_AVAILABLE)
        OCL_GetErrorString = "CL_COMPILER_NOT_AVAILABLE"
    case (CL_MEM_OBJECT_ALLOCATION_FAILURE)
        OCL_GetErrorString = "CL_MEM_OBJECT_ALLOCATION_FAILURE"
    case (CL_OUT_OF_RESOURCES)
        OCL_GetErrorString = "CL_OUT_OF_RESOURCES"
    case (CL_OUT_OF_HOST_MEMORY)
        OCL_GetErrorString = "CL_OUT_OF_HOST_MEMORY"
    case (CL_PROFILING_INFO_NOT_AVAILABLE)
        OCL_GetErrorString = "CL_PROFILING_INFO_NOT_AVAILABLE"
    case (CL_MEM_COPY_OVERLAP)
        OCL_GetErrorString = "CL_MEM_COPY_OVERLAP"
    case (CL_IMAGE_FORMAT_MISMATCH)
        OCL_GetErrorString = "CL_IMAGE_FORMAT_MISMATCH"
    case (CL_IMAGE_FORMAT_NOT_SUPPORTED)
        OCL_GetErrorString = "CL_IMAGE_FORMAT_NOT_SUPPORTED"
    case (CL_BUILD_PROGRAM_FAILURE)
        OCL_GetErrorString = "CL_BUILD_PROGRAM_FAILURE"
    case (CL_MAP_FAILURE)
        OCL_GetErrorString = "CL_MAP_FAILURE"
    case (CL_INVALID_VALUE)
        OCL_GetErrorString = "CL_INVALID_VALUE"
    case (CL_INVALID_DEVICE_TYPE)
        OCL_GetErrorString = "CL_INVALID_DEVICE_TYPE"
    case (CL_INVALID_PLATFORM)
        OCL_GetErrorString = "CL_INVALID_PLATFORM"
    case (CL_INVALID_DEVICE)
        OCL_GetErrorString = "CL_INVALID_DEVICE"
    case (CL_INVALID_CONTEXT)
        OCL_GetErrorString = "CL_INVALID_CONTEXT"
    case (CL_INVALID_QUEUE_PROPERTIES)
        OCL_GetErrorString = "CL_INVALID_QUEUE_PROPERTIES"
    case (CL_INVALID_COMMAND_QUEUE)
        OCL_GetErrorString = "CL_INVALID_COMMAND_QUEUE"
    case (CL_INVALID_HOST_PTR)
        OCL_GetErrorString = "CL_INVALID_HOST_PTR"
    case (CL_INVALID_MEM_OBJECT)
        OCL_GetErrorString = "CL_INVALID_MEM_OBJECT"
    case (CL_INVALID_IMAGE_FORMAT_DESCRIPTOR)
        OCL_GetErrorString = "CL_INVALID_IMAGE_FORMAT_DESCRIPTOR"
    case (CL_INVALID_IMAGE_SIZE)
        OCL_GetErrorString = "CL_INVALID_IMAGE_SIZE"
    case (CL_INVALID_SAMPLER)
        OCL_GetErrorString = "CL_INVALID_SAMPLER"
    case (CL_INVALID_BINARY)
        OCL_GetErrorString = "CL_INVALID_BINARY"
    case (CL_INVALID_BUILD_OPTIONS)
        OCL_GetErrorString = "CL_INVALID_BUILD_OPTIONS"
    case (CL_INVALID_PROGRAM)
        OCL_GetErrorString = "CL_INVALID_PROGRAM"
    case (CL_INVALID_PROGRAM_EXECUTABLE)
        OCL_GetErrorString = "CL_INVALID_PROGRAM_EXECUTABLE"
    case (CL_INVALID_KERNEL_NAME)
        OCL_GetErrorString = "CL_INVALID_KERNEL_NAME"
    case (CL_INVALID_KERNEL_DEFINITION)
        OCL_GetErrorString = "CL_INVALID_KERNEL_DEFINITION"
    case (CL_INVALID_KERNEL)
        OCL_GetErrorString = "CL_INVALID_KERNEL"
    case (CL_INVALID_ARG_INDEX)
        OCL_GetErrorString = "CL_INVALID_ARG_INDEX"
    case (CL_INVALID_ARG_VALUE)
        OCL_GetErrorString = "CL_INVALID_ARG_VALUE"
    case (CL_INVALID_ARG_SIZE)
        OCL_GetErrorString = "CL_INVALID_ARG_SIZE"
    case (CL_INVALID_KERNEL_ARGS)
        OCL_GetErrorString = "CL_INVALID_KERNEL_ARGS"
    case (CL_INVALID_WORK_DIMENSION)
        OCL_GetErrorString = "CL_INVALID_WORK_DIMENSION"
    case (CL_INVALID_WORK_GROUP_SIZE)
        OCL_GetErrorString = "CL_INVALID_WORK_GROUP_SIZE"
    case (CL_INVALID_WORK_ITEM_SIZE)
        OCL_GetErrorString = "CL_INVALID_WORK_ITEM_SIZE"
    case (CL_INVALID_GLOBAL_OFFSET)
        OCL_GetErrorString = "CL_INVALID_GLOBAL_OFFSET"
    case (CL_INVALID_EVENT_WAIT_LIST)
        OCL_GetErrorString = "CL_INVALID_EVENT_WAIT_LIST"
    case (CL_INVALID_EVENT)
        OCL_GetErrorString = "CL_INVALID_EVENT"
    case (CL_INVALID_OPERATION)
        OCL_GetErrorString = "CL_INVALID_OPERATION"
    case (CL_INVALID_GL_OBJECT)
        OCL_GetErrorString = "CL_INVALID_GL_OBJECT"
    case (CL_INVALID_BUFFER_SIZE)
        OCL_GetErrorString = "CL_INVALID_BUFFER_SIZE"
    case (CL_INVALID_MIP_LEVEL)
        OCL_GetErrorString = "CL_INVALID_MIP_LEVEL"
    case(CL_INVALID_GLOBAL_WORK_SIZE)
        OCL_GetErrorString = "CL_INVALID_GLOBAL_WORK_SIZE"
    case default
        OCL_GetErrorString = "unknown error code"
     end select
   end function OCL_GetErrorString

end module ocl_utils_mod
